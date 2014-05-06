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
                 )
    mat_plane = ( Color 0.6 0.6 0.6
                , Color 0.6 0.6 0.6
                , Color 1.0 1.0 1.0
                , 10.0
                , Color 0.0 0.0 0.0
                )
    mat_black_tri = ( Color 0 0 0
                    , Color 0 0 0
                    , Color 0.4 0.4 0.4
                    , 100.0
                    , Color 1 1 1
                    )
    mat_red_tri = ( Color 1 0 0
                  , Color 1 0 0
                  , Color 0.6 0.6 0.6
                  , 100.0
                  , Color 1 1 1
                  )
    mat_white_tri = ( Color 1 1 1
                    , Color 1 1 1
                    , Color 0.4 0.4 0.4
                    , 10
                    , Color 0 0 0
                    )
    --mat_triangle = ( Color 1 (215/255) 0
    --               , Color 1 (215/255) 0
    --               , Color 0 0 0
    --               , 0
    --               , Color 0 0 0
    --               )
    ----mat_triangle = ( Color (0.6, 0.6, 0.6)
    ----               , Color (0.6, 0.6, 0.6)
    ----               , Color (0.0, 0.0, 0.0)
    ----               , 0.0
    ----               , Color (0.6, 0.6, 0.6)
    ----               )
    --sfcs = [ Sphere (3, 1, 5) 2 mat_sphere
    --       --, Sphere (4, 10, 2) 1 mat_sphere
    --       --, Sphere (4, 0, 12) 1 mat_sphere
    --       --, Sphere (14, 0, 2) 1 mat_sphere
    --     ----  , Plane (-40, -1, 2) (2, -1, 2) (2, -1, -20) mat_plane
    --       , Triangle (-10, -1, -10) (10, -1, -10) (-10, 5, -10) mat_triangle
    --       , Triangle (-10, 5, -10) (10, -1, -10) (10, 5, -10) mat_triangle
    --       , Triangle (-10, -1, -10) (-10, 5, -10) (-10, 5, 10) mat_triangle
    --       , Triangle (-10, -1, -10) (-10, 5, 10) (-10, -1, 10) mat_triangle
    --       ]
    --planes' = [ Plane (-40, -1, 2) (2, -1, 2) (2, -1, -20) mat_plane 
    --         ]
    --lts = [ ((50, 20, 0), Color 0.5 0.5 0.5)
    --      , ((3, 2, 20), Color 0.2 0.2 0.2)
    --      ]
    sfcs = (Sphere (6, 6, 1.76) 0.75 mat_sphere):
           (Sphere (5, 2, 1.76) 0.75 mat_sphere):
           {-
           (Triangle (0, 0, -1) (0, 0, 0.9) (0, 6, 0.9) mat_white_tri):
           (Triangle (0, 6, 0.9) (0, 6, -1) (0, 0, -1) mat_white_tri):
           (Triangle (6, 6, 1) (6, 0, 1) (6, 0, -1) mat_white_tri):
           (Triangle (6, 0, -1) (6, 6, -1) (6, 6, 1) mat_white_tri):
           -}
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_red_tri | x <- [0,2], y <- [0,2] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_red_tri | x <- [0,2], y <- [0,2] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_red_tri | x <- [1,3], y <- [1,3] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_red_tri | x <- [1,3], y <- [1,3] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_black_tri | x <- [1,3], y <- [0,2] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_black_tri | x <- [1,3], y <- [0,2] ]
           ++
           [ Triangle (x, y, 1) (x+1, y, 1) (x+1, y+1, 1) mat_black_tri | x <- [0,2], y <- [1,3] ]
           ++
           [ Triangle (x, y, 1) (x+1, y+1, 1) (x, y+1, 1) mat_black_tri | x <- [0,2], y <- [1,3] ]
           
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
                  , antialiasing = 3
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
  getColor world ray (HitRec p n _ m@(a, _, _, _, r)) depth = color where
    World { lights = lights' 
          , ambient = ambient'
          , bbTree = bbTree'
          } = world
    refl = getScaledColor r (getReflection world ray p n depth) 1
    diff_phong = mconcat $ map (getDiffuseAndPhong ray m n p bbTree') lights'
    amb = getScaledColor a ambient' 1
    color = diff_phong `mappend` amb `mappend` refl

  getDiffuseAndPhong :: Ray3 -> Material -> Vec3 -> Pt3 -> Surfaces -> Light -> Color
  getDiffuseAndPhong (Ray3 (_, dir)) (_, d, s, bp, _) n pt bbtree (lp, l) = color where

    light_dir = normalize $ subt lp pt
    light_ray = Ray3 (pt, light_dir)
    {- Check for shadows -}
    color = case light_ray `hits` bbtree of
            Just _ -> Color 0 0 0
            Nothing -> diffuse where
              diff_scale = max 0 $ dot light_dir n

              {- Blinn Phong contribution -}
              rev_dir = normalize $ multiply dir (-1.0)
              half = normalize $ add rev_dir light_dir
              phong_scale = max 0 $ dot half n ** bp

              diffuse = getScaledColor d l diff_scale `mappend` getScaledColor s l phong_scale

  getScaledColor :: Color -> Color -> Float -> Color
  getScaledColor (Color mr mg mb) (Color lr lg lb) s = 
    Color (mr * lr * s) (mg * lg * s) (mb * lb * s)

  getReflection :: World -> Ray3 -> Pt3 -> Vec3 -> Int -> Color
  getReflection world (Ray3 (_, dir)) p n depth = color where
    refRay = Ray3 (p, subt dir $ multiply n (2 * dot dir n))
    color = rayTrace (depth - 1) world [refRay]
