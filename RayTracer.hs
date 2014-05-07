{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module RayTracer (render,flatten) where
  import Surfaces
  import Geometry3
  import Debug.Trace
  import Data.Semigroup


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
                     }

  mapT :: (a -> b) -> (a,a) -> (b,b)
  mapT f (a,b) = (f a,f b)

  render :: Width -> Height -> [Color]
  render wd ht 
     | trace (show $ makeBbt sfcs AxisX) False = error "fuck"
     | otherwise 
    = map (rayTrace reflDepth world . getRays world) pixels where
    reflDepth = 2
    pixels = [ (x,y) | y <- [0..(ht-1)], x <- [0..(wd-1)] ]
    eye' = (-4, 4, 7)
    lookAt' = (8,4,1)
    up = (0,0,1)
    w = normalize $ subt eye' lookAt'
    u = normalize $ cross up w
    v = cross w u
    mat_sphere = ( Color 0.5 0.2 0.5
                 , Color 0.5 0.2 0.5
                 , Color 1.0 1.0 1.0
                 , 100.0
                 , Color 0 0 0
                 , 1
                 , Color 0 0 0
                 )
    mat_plane = ( Color 0.6 0.6 0.6
                , Color 0.6 0.6 0.6
                , Color 1.0 1.0 1.0
                , 10.0
                , Color 0.0 0.0 0.0
                , 1
                , Color 0 0 0
                )
    mat_black_tri = ( Color 0 0 0
                    , Color 0 0 0
                    , Color 0.4 0.4 0.4
                    , 100.0
                    , Color 1 1 1
                    , 1
                    , Color 1 1 1
                    )
    mat_red_tri = ( Color 1 0 0
                  , Color 1 0 0
                  , Color 0.6 0.6 0.6
                  , 100.0
                  , Color 1 1 1
                  , 1
                  , Color 1 1 1
                  )
    mat_white_tri = ( Color 1 1 1
                    , Color 1 1 1
                    , Color 0.4 0.4 0.4
                    , 10
                    , Color 0 0 0
                    , 1
                    , Color 1 1 1
                    )
    mat_glass = ( Color 0 0 1
                , Color 0 0 1
                , Color 0.6 0.6 0.6
                , 10
                , Color 1 1 1
                , 2.3
                , Color 0 1 0
                )
    sfcs = Sphere (6, 6, 1.76) 0.75 mat_sphere:
           Sphere (5, 2, 1.76) 0.75 mat_sphere: 

           Sphere (3, 3, 3) 2 mat_glass:

           Triangle (0, 0, -1) (0, 0, 1) (0, 8, 1) mat_white_tri:
           Triangle (0, 8, 1) (0, 8, -1) (0, 0, -1) mat_white_tri:
           Triangle (8, 8, 1) (8, 0, 1) (8, 0, -1) mat_white_tri:
           Triangle (8, 0, -1) (8, 8, -1) (8, 8, 1) mat_white_tri:

           Triangle (0, 0, -1) (0, 0, 1) (8, 0, 1) mat_white_tri:
           Triangle (8, 0, 1) (8, 0, -1) (0, 0, -1) mat_white_tri:
           Triangle (8, 8, 1) (0, 8, 1) (0, 8, -1) mat_white_tri:
           Triangle (0, 8, -1) (8, 8, -1) (8, 8, 1) mat_white_tri:

           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_red_tri | x <- [0,2..6], y <- [0,2..6] ]
           ++ 
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_red_tri | x <- [0,2..6], y <- [0,2..6] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_red_tri | x <- [1,3..7], y <- [1,3..7] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_red_tri | x <- [1,3..7], y <- [1,3..7] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_black_tri | x <- [1,3..7], y <- [0,2..6] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_black_tri | x <- [1,3..7], y <- [0,2..6] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_black_tri | x <- [0,2..6], y <- [1,3..7] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_black_tri | x <- [0,2..6], y <- [1,3..7] ] 
           
    planes' = [ Plane (0, 0, -1.0) (1, 0, -1) (1, 1, -1) mat_plane ]
    amb = Color 0.1 0.1 0.1
    lts = [ ((50, 1, 100), Color 1 1 1)
          , ((4, 12, 20), Color 0.2 0.2 0.2)
          ]
    --sfcs = [Triangle (-10, 5, -10) (10, -1, -10) (10, 5, -10) (1, 215/255, 0)]
    --sfcs = [Sphere (0, 0, 0) 1 (0.5, 0.2, 0.5)]
    world = World { imgDim = (800,600)
                  , viewPlane = (8,6,4)
                  , camera = (u,v,w)
                  , eye = eye'
                  , lookAt = lookAt'
                  , surfaces = sfcs
                  , planes = planes'
                  , bbTree = makeBbt sfcs AxisX
                  , lights = lts
                  , ambient = amb
                  , antialiasing = 1 --1 for none, 3 is good
                  , softshadows = 0 --1 for none
                  }

  flatten :: [Color] -> [Float]
  flatten [] = []
  flatten (Color x y z:xs) = x:y:z:flatten xs

  rayTrace :: Int -> World -> [Ray3] -> Color
  rayTrace depth world rays = color where
    World { bbTree = bbtree 
          , planes = planes'
          } = world
    color = averageColors $ map f rays
    --hit = max (ray `hits` bbtree) (ray `planeHits` planes')
   -- color = case hit of
   --         Nothing -> Color (0,0,0)
   --         Just hitRec -> getColor world ray hitRec depth
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
  getDir vW vH vD iW iH u v w (i,j) = add w_dir $ add u_dir v_dir where
    u_dir = multiply u ((i + 0.5) * vW / iW - vW / 2)
    v_dir = multiply v ((j + 0.5) * vH / iH - vH / 2)
    w_dir = multiply w (-1 * vD)
  
  getColor :: World -> Ray3 -> HitRec -> Int -> Color
  getColor _ _ _ 0 = Color 0 0 0
  getColor world ray (HitRec p n _ m@(a, _, _, _, r, i, rf)) depth = color where
    World { lights = lights' 
          , ambient = ambient'
          , bbTree = bbTree'
          , softshadows = shdwRays
          } = world
    refr = getRefraction world ray p n i rf
    refl = getScaledColor r (getReflection world ray p n depth) 1
    diff_phong = mconcat $ map (getDiffuseAndPhong shdwRays ray m n p bbTree') lights'
    amb = getScaledColor a ambient' 1
    color = diff_phong `mappend` amb `mappend` refl `mappend` refr

  getDiffuseAndPhong :: Int -> Ray3 -> Material -> Vec3 -> Pt3 -> Surfaces -> Light -> Color
  getDiffuseAndPhong shdwRays (Ray3 (_, dir)) (_, d, s, bp, _, _, _) n pt bbtree (lp, l) 
    | shdwRays == 0 = color 
    | otherwise = ss_color where
    light_dir = normalize $ subt lp pt
    light_ray = Ray3 (pt, light_dir)
    {- Check for shadows -}
    color = case light_ray `hits` bbtree of --f light_ray bbtree light_dir dir n bp d l s
            Just _ -> Color 0 0 0
            Nothing -> diffuse where
              lamb = getScaledColor d l $ max 0 $ dot light_dir n

              {- Blinn Phong contribution -}
              rev_dir = normalize $ multiply dir (-1.0)
              half = normalize $ add rev_dir light_dir
              spec_highlight = getScaledColor s l $ max 0 $ dot half n ** bp

              diffuse = lamb `mappend` spec_highlight
    {- Check to see if near edge of a shadow -}
    {-NOTE calculating pseudo random points as per
    - http://www.altdevblogaday.com/2012/05/03/generating-uniformly-distributed-points-on-sphere/
    - http://www.cse.cuhk.edu.hk/~ttwong/papers/udpoint/udpoint.pdf
    -
    pts = [ (x,y,z) | let zs = map (/4) [-4..4] :: [Float]
                          rs = map (\a -> sqrt (1 - a*a)) zs,
                      z <- zs,
                      t <- map (/(2*pi)) [0..(2*pi)],
                      x <- map (\a -> a * cos t) rs,
                      y <- map (\a -> a * sin t) rs ] -}
    ss_color = error "ss undefined" --mconcat (map (pt
    {-f ray bbtree light_dir dir n bp d l s = 
      case ray `hits` bbtree of 
            Just _ -> Color 0 0 0
            Nothing -> diffuse where
              diff_scale = max 0 $ dot light_dir n

              {- Blinn Phong contribution -}
              rev_dir = normalize $ multiply dir (-1.0)
              half = normalize $ add rev_dir light_dir
              phong_scale = max 0 $ dot half n ** bp

              diffuse = getScaledColor d l diff_scale `mappend` getScaledColor s l phong_scale
-}
  getScaledColor :: Color -> Color -> Float -> Color
  getScaledColor (Color mr mg mb) (Color lr lg lb) s = 
    Color (mr * lr * s) (mg * lg * s) (mb * lb * s)

  getReflection :: World -> Ray3 -> Pt3 -> Vec3 -> Int -> Color
  getReflection world (Ray3 (_, dir)) p n depth = color where
    refRay = Ray3 (p, subt dir $ multiply n (2 * dot dir n))
    color = rayTrace (depth - 1) world [refRay]
  
  {- TODO: fix the kr, kg, kg constants. Figure out what non-vector t is exactly -}
  getRefraction :: World -> Ray3 -> Pt3 -> Vec3 -> Float -> Color -> Color
  getRefraction _ _ _ _ 1 _ = (Color 0 0 0)
  getRefraction world (Ray3 (_, dir)) p n i (Color ar ag ab) = color where
    refl = subt dir $ multiply n (2 * dot dir n)
    (c, kr, kg, kb, t) = if dot dir n < 0
                         then (dot n (multiply dir (-1)), 1, 1, 1, refract dir n i)
                         else 
                           let
                             t' = refract dir (multiply n (-1)) (1/i)
                             kr' = 1/ar -- These should involve an exponential
                             kg' = 1/ag
                             kb' = 1/ab
                             c' = case t' of
                                  Nothing -> 1
                                  Just s -> dot s n
                           in
                             (c', kr', kg', kb', t')
    r0 = (i - 1) ** 2 / (i + 1) ** 2
    f = r0 + (1 - r0) * (1 - c) ** 5
    color = case t of 
            Nothing -> 
              let
                (Color r g b) = rayTrace 1 world [(Ray3 (p, refl))]
              in
                (Color (kr*r) (kg*g) (kb*b))
            Just s -> 
              let
                (Color r g b) = (getScaledColor (rayTrace 1 world [(Ray3 (p, refl))]) (Color 1 1 1) f) `mappend`
                  (getScaledColor (rayTrace 1 world [(Ray3 (p, s))]) (Color 1 1 1) (1 - f))
              in
                (Color (kr*r) (kg*g) (kb*b))

  {- We assume that objects are embedded in air -}
  {- TODO: Adjust to account for other mediums -}
  refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3
  refract d n i = if internal_ref < 0
                  then Nothing
                  else Just t where
                      internal_ref = sqrt $ 1 - (i * i * (1 - (dot d n) ** 2))
                      t = subt (multiply (subt d (multiply n (dot d n))) i) (multiply n internal_ref)
