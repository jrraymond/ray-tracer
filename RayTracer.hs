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
                     , focus :: Float
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
    f = averageColors . (map $ rayTrace refldepth world) . getRays world
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

  getRays :: World -> (Int,Int) -> [[Ray3]]
  getRays world pixel_coords = rays where
    World { imgDim = img_dim
          , viewPlane = (viewWd',viewHt',viewDist)
          , camera = (u,v,w) 
          , eye = base
          , antialiasing = aa  
          , rng = rng' 
          } = world
    (i,j) = mapT fromIntegral pixel_coords
    (viewWd,viewHt) = mapT fromIntegral (viewWd',viewHt')
    (imgWd,imgHt) = mapT fromIntegral img_dim
    f = (/ fromIntegral aa) . fromIntegral
    xs = [ (i + i',j + j') | i' <- map f [0..aa] , j' <- map f [0..aa] ]
    rays = map ((\d -> getDOF rng' 25 1 u v (Ray3 (base, d))) . (getDir viewWd viewHt viewDist imgWd imgHt base u v w)) xs 
    --dirs = getDir viewWd viewHt viewDist imgWd imgHt u v w (i,j)

  {- We changed getDir in the following way,
   - instead of getting just the normalized direction, it returns
   - the full direction to the viewing plane so that
   - we can send other rays through this point (which we then normalize)
   -}

  getDir :: Float -> Float -> Float -> Float -> Float -> Pt3 -> Vec3 -> Vec3 -> Vec3 -> (Float,Float) -> Vec3
  getDir vW vH vD iW iH e u v w (i,j) = add w_dir $ add u_dir v_dir where
    u_dir = multiply u ((i + 0.5) * vW / iW - vW / 2)
    v_dir = multiply v ((j + 0.5) * vH / iH - vH / 2)
    w_dir = multiply w (-1 * vD)

  --getDir :: Float -> Float -> Float -> Float -> Float -> Pt3 -> Vec3 -> Vec3 -> Vec3 -> (Float,Float) -> Vec3
  --getDir vW vH vD iW iH e u v w (i,j) = subt (add w_dir $ add u_dir v_dir) e where
  --  u_dir = multiply u ((i + 0.5) * vW / iW - vW / 2)
  --  v_dir = multiply v ((j + 0.5) * vH / iH - vH / 2)
  --  w_dir = multiply w (-1 * vD)

  {- Depth of field works by jittering the base of the eye point (for each pixel)
   - and sending all those rays through the same point on 
   - the focus plane (usually the viewing window) and averaging out.
   -
   - A pixel is in focus if it lies on the focus plane since
   - each jittered ray will converge on that point before splitting
   - off into their various directions.
   -
   - To disable, comment body of the function and uncomment the
   - currently comment out line. (This should work).
   -}
  getDOF :: StdGen -> Int -> Float -> Vec3 -> Vec3 -> Ray3 -> [Ray3]
  getDOF rng samples radius u v (Ray3 (e, dir)) = rays where
    rays = [(Ray3 (e, normalize dir))]
    --p = add e dir 
    --(ls, rs) = splitAt samples $ take (2*samples) $ randomRs (-radius, radius) rng
    --rays = map (getDOFRays e p u v) (zip ls rs)

  getDOFRays :: Pt3 -> Pt3 -> Vec3 -> Vec3 -> (Float, Float) -> Ray3
  getDOFRays e p u v (u', v') = ray where
    e' = add e $ add (multiply u u') (multiply v v')
    dir = normalize $ subt p e'
    ray = Ray3 (e', dir)


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

  {- Computes the reflection. If an object is glossy, then
   - it will do a distributed ray trace to calculate 
   - glossy reflection. Glossy reflection works by taking
   - a ray whose direction is a slight perturbation from 
   - ideal specular reflection. The distributed part smooths
   - the glossiness out to make it look more natural.
   -
   - Note: Planes have a large problem with our current implementation
   - (Our perturbations are not the best) so we recommend
   - not having glossy planes. 
   -
   - Note: This may require a huge amount of computation since
   - each distributed ray may reflect and therefore spew of another
   - set of rays.
   -
   - The commented out function does not incorporate glossiness.
   -}
  getReflection :: World -> Ray3 -> Pt3 -> Vec3 -> Float -> Int -> Color
  getReflection world (Ray3 (_, dir)) p n g depth = rayTrace (depth - 1) world [Ray3 (p, normalize $ subt dir $ multiply n (2 * dot dir n))]
  --getReflection world (Ray3 (_, dir)) p n 0 depth = rayTrace (depth - 1) world [Ray3 (p, normalize $ subt dir $ multiply n (2 * dot dir n))]
  --getReflection world (Ray3 (_, dir)) p n g depth = rayTrace (depth - 1) world $ getReflectionRays world dir p n g
  {-See note below-}

  {- This gets a sample of rays to smooth out the glossiness. -}
  getReflectionRays :: World -> Vec3 -> Pt3 -> Vec3 -> Float -> [Ray3]
  getReflectionRays world dir p n g = rays where
    World { rng = prng } = world
    {- If you want smoother gloss, increase this number.
     - There is an issue with calculating the jittered ray.
     - It sometimes goes under the surface which will cause problems (infinite loops).
     - It seems to work fine if you don't make planes glossy
     -}
    num_rays = 8
    xs = take (2*num_rays) $ randomRs (-g/2, g/2) (prng)
    coords = zip (take num_rays xs) (drop num_rays xs)
    
    rays = map (getRefRay dir p n g) coords

  {- Computes a perturbed ray. This uses the basic scheme found in
   - chapter 2.4.6 to find a random perturbation.
   - Essentially we take a small square centered at the origin
   - whose size is based on the glossiness factor and find a random
   - coordinate and adjust the ray direction by that amount along
   - a basis of the reflection.
   -}
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
    refdir = normalize $ add r $ add (multiply u u') (multiply v v')
    ray = if dot refdir n < 0
          then Ray3 (p, r)
          else Ray3 (p, normalize $ add r $ add (multiply u u') (multiply v v'))

  {- Compute the refraction of the object. 
   - If the refraction index is 0, then there is nothing to do.
   - Otherwise, we compute the refraction of the object.
   -}
  getRefraction :: World -> Ray3 -> Pt3 -> Vec3 -> Float -> Color -> Color
  getRefraction _ _ _ _ 0 _ = (Color 0 0 0)
  getRefraction world (Ray3 (_, dir)) p n i (Color ar ag ab) = color where
    World { reflDepth = depth } = world

    {- Compute the reflection direction. -}
    refl = normalize $ subt dir $ multiply n (2 * dot dir n)

    {- A bunch of constants. 
     - c = the angle of incidence (the direction of the viewing ray with respect to normal)
     - kr, kg, kb = the attenuation constants.
     - t = the transformed vector
     -
     - We check if we are entering or leaving the surface.
     - If we are entering, the dot product is negative
     - and we compute the refraction.
     - Otherwise we are leaving. Since we assume all objects are in air,
     - we simple take the reciprocal of the index and calculate attenuation.
     - (Note, the attenuation might not be completely accurate)
     -}
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
    
    {- We use the Schlick approximation of the Fresnel equations
     - to determine how much contribution is reflection
     - and how much is refraction.
     -}
    reflectance = (i - 1) ** 2 / (i + 1) ** 2
    schlick = reflectance + (1 - reflectance) * (1 - c) ** 5
    color = case t of 
            Nothing -> 
              let
                {- Total internal reflection. Contribution is reflection only. -}
                (Color r g b) = rayTrace depth world [(Ray3 (p, refl))]
              in
                (Color (kr*r) (kg*g) (kb*b))
            Just s -> 
              let
                (Color r g b) = (getScaledColor (rayTrace 3 world [(Ray3 (p, refl))]) (Color 1 1 1) schlick) `mappend`
                    (getScaledColor (rayTrace 3 world [(Ray3 (p, s))]) (Color 1 1 1) (1 - schlick))
              in
                (Color (kr*r) (kg*g) (kb*b))

  {- We assume that objects are embedded in air 
   - Returns Nothing if there is total internal refraction, ie the
   - ray bounces around inside the object and has no contribution, 
   - otherwise returns the transformed direction. 
   -}
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

