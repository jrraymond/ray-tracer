{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module RayTracer (render
                 , flatten
                 , World (World)) where
  import Surfaces
  import Geometry3
  --import Debug.Trace
  import Data.Semigroup
  import Data.Maybe (catMaybes)
  import Control.Parallel.Strategies hiding (dot)
  import System.Random

  type Width = Int
  type Height = Int

  data World = World { imgDim :: (Width,Height)
                     , viewPlane :: (Width,Height,Float)
                     , camera :: (Vec3,Vec3,Vec3)
                     , eye :: Pt3
                     , lookAt :: Pt3
                     , surfaces :: [Shape]
                     , planes :: [Shape]
                     , bbTree :: Surfaces
                     , lights :: [Light]
                     , ambient :: Color
                     , antialiasing :: Int
                     , softshadows :: Int
                     , reflDepth :: Int
                     , rng :: StdGen
                     }

  mapT :: (a -> b) -> (a,a) -> (b,b)
  mapT f (a,b) = (f a,f b)

  render :: World -> [Color]
  render world = ps' where
    refldepth = reflDepth world
    f = rayTrace refldepth world . getRays world
    pixels = [ (x,y) | y <- [0..(ht-1)], x <- [0..(wd-1)] ]
    (wd,ht) = imgDim world
    ps' = map f pixels `using` parListChunk 500 rseq
  flatten :: [Color] -> [Float]
  flatten [] = []
  flatten (Color x y z:xs) = x:y:z:flatten xs

  rayTrace :: Int -> World -> [Ray3] -> Color
  rayTrace depth world rays = color where
    bbtree = bbTree world
    planes' = planes world
    color = averageColors $ map f rays
    f r = case max (r `hits` bbtree) (r `planeHits` planes') of
            Nothing -> Color 0 0 0
            Just hitRec -> getColor world r hitRec depth
  
  averageColors ::[Color] -> Color
  averageColors cs = f cs (Color 0 0 0) 0 where
    f :: [Color] -> Color -> Float -> Color
    f [] (Color r g b) n = Color (r/n) (g/n) (b/n)
    f (Color r' g' b':xs) (Color r g b) n = f xs (Color (r+r') (g+g') (b+b')) (n+1)

  getRays :: World -> (Int,Int) -> [Ray3]
  getRays world pixel_coords = rays where
    World { imgDim = img_dim
          , viewPlane = (viewWd',viewHt',viewDist)
          , camera = (u,v,w) 
          , eye = base
          , antialiasing = aa  } = world
    (i,j) = mapT fromIntegral pixel_coords
    (viewWd,viewHt) = mapT fromIntegral (viewWd',viewHt')
    (imgWd,imgHt) = mapT fromIntegral img_dim
    f = (/ fromIntegral aa) . fromIntegral
    xs = [ (i + i',j + j') | i' <- map f [0..aa] , j' <- map f [0..aa] ]
    rays = map ((\d -> Ray3 (base,d)) . (getDir viewWd viewHt viewDist imgWd imgHt u v w)) xs 
    --dirs = getDir viewWd viewHt viewDist imgWd imgHt u v w (i,j)

  getDir :: Float -> Float -> Float -> Float -> Float -> Vec3 -> Vec3 -> Vec3 -> (Float,Float) -> Vec3
  getDir vW vH vD iW iH u v w (i,j) = normalize $ add w_dir $ add u_dir v_dir where
    u_dir = multiply u ((i + 0.5) * vW / iW - vW / 2)
    v_dir = multiply v ((j + 0.5) * vH / iH - vH / 2)
    w_dir = multiply w (-1 * vD)
  
  getColor :: World -> Ray3 -> HitRec -> Int -> Color
  getColor _ _ _ 0 = Color 0 0 0
  getColor world ray (HitRec p n _ m@(a, _, _, _, r, i, rf, g)) depth = color where
    World { lights = lights' 
          , ambient = ambient'
          , bbTree = bbTree'
          , softshadows = shdwRays
          } = world
    refr = getRefraction world ray p n i rf
    refl = getScaledColor r (getReflection world ray p n g depth) 1
    diff_phong = mconcat $ map (getDiffuseAndPhong shdwRays ray m n p bbTree') lights'
    amb = getScaledColor a ambient' 1
    color = diff_phong `mappend` amb `mappend` refl `mappend` refr
  
  getDiffuseAndPhong :: Int -> Ray3 -> Material -> Vec3 -> Pt3 -> Surfaces -> Light -> Color
  getDiffuseAndPhong shdwRays (Ray3 (_,rdir)) (_, d, s, bp, _, _, _, _) n pt bbtree (lp, l) 
    | shdwRays == 0 = f (Ray3 (pt,light_dir),light_dir)
    | otherwise = ss_color where
    light_pts = map (add lp) $ getHammerslayPoints 8 -- test with 8
    ssdirs = map (normalize . flip subt pt) light_pts
    ssrays = map Ray3 $ zip (repeat pt) ssdirs
    numhits = length $ catMaybes $ map (`hits` bbtree) ssrays
    light_pts' = map (add lp) $ getHammerslayPoints 128 -- if on edge need 128 rays
    ssdirs' = map (normalize . flip subt pt) light_pts'
    ssrays' = map Ray3 $ zip (repeat pt) ssdirs'
    ss_color = if numhits > 0 && numhits < 8 
                 then averageColors $ map f $ zip ssrays' ssdirs' --on edge
                 else averageColors $ map f $ zip ssrays ssdirs 
    f (ssray,ssdir) = case ssray `hits` bbtree of
              Just _ -> Color 0 0 0 
              Nothing -> diffuse where
                lamb = getScaledColor d l $ max 0 $ dot ssdir n
                {- Blinn Phong contribution -}
                rev_dir = normalize $ multiply rdir (-1.0)
                half = normalize $ add rev_dir ssdir
                spec_highlight = getScaledColor s l $ max 0 $ dot half n ** bp
                diffuse = lamb `mappend` spec_highlight
    light_dir = normalize $ subt lp pt
    {- Check to see if near edge of a shadow -}
    {-NOTE calculating pseudo random points as per
    - http://www.altdevblogaday.com/2012/05/03/generating-uniformly-distributed-points-on-sphere/
    - http://www.cse.cuhk.edu.hk/~ttwong/papers/udpoint/udpoint.pdf -}
  getScaledColor :: Color -> Color -> Float -> Color
  getScaledColor (Color mr mg mb) (Color lr lg lb) s = 
    Color (mr * lr * s) (mg * lg * s) (mb * lb * s)

  getReflection :: World -> Ray3 -> Pt3 -> Vec3 -> Float -> Int -> Color
  getReflection world (Ray3 (_, dir)) p n g depth = rayTrace (depth - 1) world [Ray3 (p, normalize $ subt dir $ multiply n (2 * dot dir n))]
  --getReflection world (Ray3 (_, dir)) p n g depth = rayTrace (depth - 1) world $ getReflectionRays world dir p n g
  {-See note below-}

  getReflectionRays :: World -> Vec3 -> Pt3 -> Vec3 -> Float -> [Ray3]
  getReflectionRays world dir p n g = rays where
    World { rng = prng } = world
    {- If you want smoother gloss, increase this number.
     - There is an issue with calculating the jittered ray.
     - It sometimes goes under the surface which will cause problems (infinite loops).
     -}
    num_rays = 1
    xs = take (2*num_rays) $ randomRs (-g/2, g/2) (prng)
    coords = zip (take num_rays xs) (drop num_rays xs)
    
    rays = map (getRefRay dir p n g) coords

  getRefRay :: Vec3 -> Pt3 -> Vec3 -> Float -> (Float, Float) -> Ray3
  getRefRay dir p n g (xi, xi') = ray where
    r@(rx, ry, rz) = normalize $ subt dir $ multiply n (2 * dot dir n)
    t = if abs rx < abs ry && abs rx < abs rz
        then (1, ry, rz)
        else if abs ry < abs rz
        then (rx, 1, rz)
        else (rx, ry, 1)
    u = normalize $ cross t r
    v = cross r u

    u' = -g / 2 + xi * g
    v' = -g / 2 + xi' * g
    ray = Ray3 (p, normalize $ add r $ add (multiply u u') (multiply v v'))

  {- TODO: fix the kr, kg, kg constants. Figure out what non-vector t is exactly -}
  getRefraction :: World -> Ray3 -> Pt3 -> Vec3 -> Float -> Color -> Color
  getRefraction _ _ _ _ 0 _ = (Color 0 0 0)
  getRefraction world (Ray3 (_, dir)) p n i (Color ar ag ab) = color where
    World { reflDepth = depth } = world
    refl = normalize $ subt dir $ multiply n (2 * dot dir n)
    (c, kr, kg, kb, t) = if dot dir n < 0
                         then ((-1) * (dot dir n), 1, 1, 1, refract dir n i)
                         else 
                           let
                             t' = refract dir (multiply n (-1)) (1/i)
                             --w = magnitude $ subt e p
                             kr' = exp(-1 * log(ar))
                             kg' = exp(-1 * log(ag))
                             kb' = exp(-1 * log(ab))
                             c' = case t' of
                                  Nothing -> 0
                                  Just s -> dot s n
                           in
                             (c', kr', kg', kb', t')
    
    reflectance = (i - 1) ** 2 / (i + 1) ** 2
    schlick = reflectance + (1 - reflectance) * (1 - c) ** 5
    color = case t of 
            Nothing -> 
              let
                
                (Color r g b) = rayTrace depth world [(Ray3 (p, refl))]
              in
                (Color (kr*r) (kg*g) (kb*b))
            Just s -> 
              let
                (Color r g b) = (getScaledColor (rayTrace 3 world [(Ray3 (p, refl))]) (Color 1 1 1) schlick) `mappend`
                    (getScaledColor (rayTrace 3 world [(Ray3 (p, s))]) (Color 1 1 1) (1 - schlick))
              in
                (Color (kr*r) (kg*g) (kb*b))

  {- We assume that objects are embedded in air -}
  {- TODO: Adjust to account for other mediums -}
  refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3
  refract d n i = if internal_ref < 0
                  then Nothing
                  else Just t where
                      internal_ref = sqrt $ 1 - (i * i * (1 - (dot d n) ** 2))
                      t = normalize $ subt (multiply (subt d (multiply n (dot d n))) i) (multiply n internal_ref)


  getHammerslayPoints :: Int -> [(Float,Float,Float)]
  getHammerslayPoints n = ps where
    ks = [ k | k <- [0..n-1] ]
    dos :: (Integral a) => a -> a
    dos = ceiling . (logBase 2 :: Float -> Float) . fromIntegral . (1+)
    ps = map (g . f) ks
    g :: (Int,Float) -> (Float,Float,Float)
    g (k,t) = (st * cos phirad,st * sin phirad,z) where
      z = 2 * t - 1
      phi = ((fromIntegral k) + 0.5) / fromIntegral n
      phirad = phi * 2 * pi
      st = sqrt (1 - z**2)
    f :: Int -> (Int,Float)
    f k = let (_,_,t') = (iterate h (k,0.5,0)) !! (dos k) in (k,t')
    h :: (Int,Float,Float) -> (Int,Float,Float)
    h (k,p,t)  
      | odd k =  (k `div` 2,p * 0.5 ,t + p)
      | otherwise = (k `div` 2,p * 0.5,t)

