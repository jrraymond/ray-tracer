module Surfaces 

  where
  import Geometry3
  import Data.Monoid
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

  data Surface = Bbox Surface Surface Shape | Leaf Shape Material
  data Shape = Sphere Pt3 Float Color
               | Triangle Pt3 Pt3 Pt3 Color
               | Plane Pt3 Pt3 Pt3 Color
               | Box Float Float Float Float Float Float 
  type Material = ()


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
