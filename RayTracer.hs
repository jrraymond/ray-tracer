{-# LANGUAGE FlexibleInstances #-}
module RayTracer (render,flatten) where
  import Surfaces
  import Geometry3
  import Data.Monoid
  import Debug.Trace


  type Width = Int
  type Height = Int
  --type Color = (Float , Float , Float)

  data World = World { imgDim :: (Width,Height),
                       viewPlane :: (Width,Height,Float),
                       camera :: (Vec3,Vec3,Vec3),
                       eye :: Pt3,
                       lookAt :: Pt3,
                       surfaces :: [Shape]}
                       --lights :: [Light]} TODO

  mapT :: (a -> b) -> (a,a) -> (b,b)
  mapT f (a,b) = (f a,f b)
  {- alternitiely mapT = join (***) but this is rather
  - opaque. join takes a->a->b and a->b, passes the argument to 
  - both positions of a->a->b
  - :t join ~> (a->a->b)->a->b or (a->)((a->)b)->(a->)b
  - seriously why would anyone write it like that
  -}

  render :: Width -> Height -> [(Float,Float,Float)]
  render wd ht 
  --  | trace (show u ++ "|" ++ show v ++ "|" ++ show w) False = undefined
  --  | otherwise = map (rayTrace world) $ map (getRay world) pixels where
    = map (rayTrace world) $ map (getRay world) pixels where
    pixels = [ (x,y) | y <- [0..(ht-1)], x <- [0..(wd-1)] ]
    --eye = (4,4,4)
    eye = (25, 2, 25)
    lookAt = (-1,-1,-1)
    up = (0,1,0)
    w = normalize $ subt eye lookAt
    u = normalize $ cross up w
    v = cross w u
    sfcs = [ Sphere (0, 0, 0) 1 (0.5, 0.2, 0.5)
           , Plane (-40, -1, 2) (2, -1, 2) (2, -1, -20) (0.6, 0.6, 0.6)
           , Triangle (-10, -1, -10) (10, -1, -10) (-10, 5, -10) (1, 215/255, 0)
           , Triangle (-10, 5, -10) (10, -1, -10) (10, 5, -10) (1, 215/255, 0)
           , Triangle (-10, -1, -10) (-10, 5, -10) (-10, 5, 10) (1, 215/255, 0)
           , Triangle (-10, -1, -10) (-10, 5, 10) (-10, -1, 10) (1, 215/255, 0)
           ]
    world = World { imgDim = (800,600), viewPlane = (8,6,7),
                    camera = (u,v,w),
                    eye = eye,
                    lookAt = lookAt,
                    surfaces = sfcs--[Sphere (0,0,0) 1]
                  }
  
  flatten :: [(Float,Float,Float)] -> [Float]
  flatten [] = []
  flatten ((x,y,z):xs) = x:y:z:flatten xs

  rayTrace :: World -> Ray3 -> Color
  rayTrace world ray = color where
    World { surfaces = surfaces } = world
    intersection = mconcat $ map (intersect ray) surfaces
    color = case intersection of
              Nothing -> (0,0,0)
              Just (HitRec (_, _, _, c)) -> c--(1,1,1) -- if trace (show hr) False then (1,1,1) else (1,1,1)
  
  getRay :: World -> (Int,Int) -> Ray3
  getRay world pixel_coords 
    | trace ("i:" ++ show i ++ " j: " ++ show j ++ "\t" ++ show (u_world,v_world,w_world) ++ "   " ++ show dir) False = undefined
   | otherwise = Ray3 (base , dir) where
    World { imgDim = img_dim,
            viewPlane = (viewWd',viewHt',viewDist),
            camera = (u,v,w) ,
            eye = base } = world
    (i,j) = mapT fromIntegral pixel_coords
    (viewWd,viewHt) = mapT fromIntegral (viewWd',viewHt')
    (imgWd,imgHt) = mapT fromIntegral img_dim
    u_world = (i+0.5)*viewWd/imgWd - viewWd/2.0
    v_world = (j+0.5)*viewHt/imgHt - viewHt/2.0
    w_world = -1.0*viewDist
    u_dir = multiply u u_world
    v_dir = multiply v v_world
    w_dir = multiply w w_world
    dir = add w_dir $ add u_dir v_dir
