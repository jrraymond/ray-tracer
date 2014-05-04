module Surfaces 

  where
  import Geometry3
  import Data.Semigroup
  import Debug.Trace (trace)

  type Color = (Float , Float , Float)
  {- Hit records have an intersection point, a normal, and a time -}  
  newtype HitRec = HitRec (Pt3 , Vec3 , Float, Color) deriving Show

  {- Semigroup for hitrec TODO using semigroup instead-}
  instance Monoid HitRec where
    h1@(HitRec (_,_,t1,_)) `mappend` h2@(HitRec (_,_,t2,_))
      | t1 < t2 = h1 
      | otherwise = h2


--  (<>) :: Monoid a => a -> a -> a
--  (<>) = mappend
--  infixr 6 <>

  data Surfaces = Node Surfaces Surfaces Shape | Leaf Shape Material | Empty
  data Shape = Sphere Pt3 Float Color
             | Triangle Pt3 Pt3 Pt3 Color
             | Plane Pt3 Pt3 Pt3 Color
             | Box Float Float Float Float Float Float 
  type Material = ()

  {- Shapes form a semigroup with respect to make a bounding box out of them -}
  instance Semigroup Shape where
    (Sphere (x,y,z) r _) <> (Sphere (x',y',z') r' _) = Box l rt b t n f where
      l  = min (x-r) (x'-r')
      rt = max (x+r) (x'+r')
      b  = min (y-r) (y'-r')
      t  = max (y+r) (y'+r')
      n  = min (z-r) (z'-r')
      f  = max (z+r) (z'+r')
    (Sphere (x,y,z) r _) <> (Triangle (ax,ay,az) (bx,by,bz) (cx,cy,cz) _) = Box l rt b t n f where
      l = min (x-r) $ min ax $ min bx cx
      rt = min (x-r) $ min ax $ min bx cx
      b = min (y-r) $ min ay $ min by cy
      t = min (y-r) $ min ay $ min by cy
      n = min (z-r) $ min az $ min bz cz
      f = min (z-r) $ min az $ min bz cz
    t@(Triangle _ _ _ _) <> s@(Sphere _ _ _) = s <> t
    _ <> _ = error "attempted to make bounding box around plane or box"

  makeBoundingHierarchy :: [Shape] -> Surfaces
  makeBoundingHierarchy [] = Empty
  makeBoundingHierarchy (s:[]) = Leaf s ()
  makeBoundingHeirarchy ss = Node left right bbox where
    left = Empty
    right = Empty
    bbox = sconcat ss

  getBox :: Shape -> Shape
  getBox (Sphere (x,y,z) r _) = Box (x-r) (x+r) (y-r) (y+r) (z-r) (z+r)
  getBox (Triangle (ax,ay,az) (bx,by,bz) (cx,cy,cz) _) = Box l r b t n f where
    l = min ax $ min bx cx
    r = max ax $ max bx cx
    b = min ay $ min by cy
    t = max ay $ max by cy
    n = min az $ min bz cz
    f = max az $ max bz cz
  getBox _ = error "Attempted to create bounding box around plane or bounding box"

  


  hits :: Ray3 -> Surfaces -> Maybe HitRec
  hits ray (Leaf shape material) = intersect ray shape 
  hits ray (Node left right bbox) = 
    case intersect ray bbox of
      Nothing -> Nothing
      _ -> (hits ray left) `mappend` (hits ray right)

  -- Surface intersection functions
  intersect :: Ray3 -> Shape -> Maybe HitRec
  intersect (Ray3 (base , dir)) (Sphere center radius color) = hitRec where
    ec = subt base center
    dec = dot dir ec
    dirdir = dot dir dir
    ecec = dot ec ec
    discriminant = dec*dec - dirdir*(ecec - radius*radius)
    disc_val = sqrt discriminant
    neg_dir = multiply dir (-1.0)
    neg_dir_ec = dot neg_dir ec
    t_a = (neg_dir_ec + disc_val) / dirdir
    t_b = (neg_dir_ec - disc_val) / dirdir
    t = min t_a t_b
    pt = add (multiply dir t) base
    n = normalize $ subt pt center
    hitRec = if discriminant <= 0 || t < 0 
               then Nothing 
               else Just (HitRec (pt , n , t, color)) 
  {- Intersection with a triangle -}
  intersect (Ray3 (base, dir)) (Triangle ta tb tc color) = hitRec where
    (base_x,base_y,base_z) = base
    (g, h, i) = dir
    (ax, ay, az) = ta
    (bx, by, bz) = tb
    (cx, cy, cz) = tc

    (a, b, c) = (ax - bx, ay - by, az - bz) 
    (d, e, f) = (ax - cx, ay - cy, az - cz)
    (j, k, l) = (ax - base_x, ay - base_y, az - base_z)
    
    (ei, hf, gf, di, dh, eg) = (e*i, h*f, g*f, d*i, d*h, e*g)
    (ak, jb, jc, al, bl, kc) = (a*k, j*b, j*c, a*l, b*l, k*c)

    (ei_hf, gf_di, dh_eg) = (ei - hf, gf - di, dh - eg)
    (ak_jb, jc_al, bl_kc) = (ak - jb, jc - al, bl - kc)

    m = a*ei_hf + b*gf_di + c*dh_eg
    beta = (j*ei_hf + k*gf_di + l*dh_eg) / m
    gamma = (i*ak_jb + h*jc_al + g*bl_kc) / m

    -- if I put the neg in t < 0, which is not right
    t = (f*ak_jb + e*jc_al + d*bl_kc) / m 
    pt = add base $ multiply dir t
    n = normalize $ cross (subt tb ta) (subt tc ta)

    hitRec = if gamma < 0 || gamma > 1 || beta < 0 || beta + gamma > 1 -- beta < 0 || beta > 1 || gamma < 0 || beta+gamma > 1-- || t < 0
             then Nothing 
             else Just (HitRec (pt , n , t, color)) 
  {- Intersection with a plane -}
  intersect (Ray3 (base, dir)) (Plane a b c color) = hitRec where
    v1 = subt b a
    v2 = subt c a
    n = normalize $ cross v1 v2
    
    a_e = subt a base

    t = dot a_e n / dot dir n
    pt = add (multiply dir t) base

    hitRec = if t < 0
             then Nothing
             else Just (HitRec (pt, n, t, color))
  {- Intersection with a box -}
  intersect (Ray3 ((bx,by,bz),(dx,dy,dz))) (Box l r b t n f) 
    | t_x0 > t_y1 || t_x0 > t_z1 || t_x1 < t_y0 || t_x1 < t_z0 ||
      t_y0 > t_z1 || t_y1 < t_z0  = Nothing
    | otherwise = Just (HitRec ((0,0,0),(0,0,0),0,color))  where
    (t_x0,t_x1) = if dx >= 0 then ((l - bx) / dx , (r - bx) / dx) 
                             else ((r - bx) / dx , (l - bx) / dx)
    (t_y0,t_y1) = if dy >= 0 then ((b - by) / dy , (t - by) / dy)
                             else ((t - by) / dy , (b - by) / dy)
    (t_z0,t_z1) = if dz >= 0 then ((n - bz) / dz , (n - bz) / dz)
                             else ((f - bz) / dz , (f - bz) / dz)
    color = (0,0,0)
