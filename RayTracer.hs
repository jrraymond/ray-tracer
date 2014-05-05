{-# LANGUAGE FlexibleInstances #-}
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
                     , bbTree :: Surfaces
                     , lights :: [Light]
                     , ambient :: Color
                     }

  mapT :: (a -> b) -> (a,a) -> (b,b)
  mapT f (a,b) = (f a,f b)
  {- alternitiely mapT = join (***) but this is rather
  - opaque. join takes a->a->b and a->b, passes the argument to 
  - both positions of a->a->b
  - :t join ~> (a->a->b)->a->b or (a->)((a->)b)->(a->)b
  - seriously why would anyone write it like that
  -}

  render :: Width -> Height -> [Color]
  render wd ht 
    | trace (show $ makeBbt sfcs AxisX) False = error "fuck"
    | otherwise 
    = map (rayTrace world . getRay world) pixels where
    pixels = [ (x,y) | y <- [0..(ht-1)], x <- [0..(wd-1)] ]
    eye' = (25, 2, 25)
    lookAt' = (-1,-1,-1)
    up = (0,1,0)
    w = normalize $ subt eye' lookAt'
    u = normalize $ cross up w
    v = cross w u
    mat_sphere = ( Color (0.5, 0.2, 0.5)
                 , Color (0.5, 0.2, 0.5)
                 , Color (1.0, 1.0, 1.0)
                 , 1000.0
                 , Color (0,0,0)
                 )
    --mat_plane = ( Color (0.6, 0.6, 0.6)
    --            , Color (0.6, 0.6, 0.6)
    --            , Color (0.0, 0.0, 0.0)
    --            , 0.0
    --            , Color (0.6, 0.6, 0.6)
    --            )
    mat_triangle = ( Color (1, 215/255, 0)
                   , Color (1, 215/255, 0)
                   , Color (0, 0, 0)
                   , 0
                   , Color (0,0,0)
                   )
    sfcs = [ Sphere (3, 1, 5) 2 mat_sphere
           , Sphere (4, 10, 2) 1 mat_sphere
           , Sphere (4, 0, 12) 1 mat_sphere
           , Sphere (14, 0, 2) 1 mat_sphere
         --  , Plane (-40, -1, 2) (2, -1, 2) (2, -1, -20) mat_plane
           , Triangle (-10, -1, -10) (10, -1, -10) (-10, 5, -10) mat_triangle
           , Triangle (-10, 5, -10) (10, -1, -10) (10, 5, -10) mat_triangle
           , Triangle (-10, -1, -10) (-10, 5, -10) (-10, 5, 10) mat_triangle
           , Triangle (-10, -1, -10) (-10, 5, 10) (-10, -1, 10) mat_triangle
           ]
    lts = [ ((50, 20, 0), Color (0.5, 0.5, 0.5))
          , ((3, 2, 20), Color (0.2, 0.2, 0.2))
          ]
    amb = Color (0.1, 0.1, 0.1)
    --sfcs = [Triangle (-10, 5, -10) (10, -1, -10) (10, 5, -10) (1, 215/255, 0)]
    --sfcs = [Sphere (0, 0, 0) 1 (0.5, 0.2, 0.5)]
    world = World { imgDim = (800,600), viewPlane = (8,6,8)
                  , camera = (u,v,w)
                  , eye = eye'
                  , lookAt = lookAt'
                  , surfaces = sfcs
                  , bbTree = makeBbt sfcs AxisX
                  , lights = lts
                  , ambient = amb
                  }
  
  flatten :: [Color] -> [Float]
  flatten [] = []
  flatten (Color (x,y,z):xs) = x:y:z:flatten xs

  rayTrace :: World -> Ray3 -> Color
  rayTrace world ray = color where
    World { bbTree = bbtree } = world
    color = case ray `hits` bbtree of
              Nothing -> Color (0,0,0)
              Just hitRec -> getColor world ray hitRec
  
  getRay :: World -> (Int,Int) -> Ray3
  getRay world pixel_coords = Ray3 (base , dir) where
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


  getColor :: World -> Ray3 -> HitRec -> Color
  getColor world ray (HitRec (p, n, _, m@(a, _, _, _, _))) = color where
    World { lights = lights' 
          , ambient = ambient'
          } = world
    {- Is there a way to make this one iteration over the lights? -}
    dif = mconcat $ map (getDiffuseAndPhong ray m n p) lights'
    amb = getScaledColor a ambient' 1
    color = dif `mappend` amb

  getDiffuseAndPhong :: Ray3 -> Material -> Vec3 -> Pt3 -> Light -> Color
  getDiffuseAndPhong (Ray3 (_, dir)) (_, d, s, bp, _) n pt (lp, l) = color where
    {- Diffuse contrinution -}
    light_dir = normalize $ subt lp pt
    diff_scale = max 0 $ dot light_dir n

    {- Blinn Phong contribution -}
    rev_dir = normalize $ multiply dir (-1.0)
    half = normalize $ add rev_dir light_dir
    phong_scale = (max 0 $ dot half n) ** bp

    color = (getScaledColor d l diff_scale) `mappend` (getScaledColor s l phong_scale)

  getScaledColor :: Color -> Color -> Float -> Color
  getScaledColor (Color (mr, mg, mb)) (Color (lr, lg, lb)) s = 
    Color (mr * lr * s, mg * lg * s, mb * lb * s)
