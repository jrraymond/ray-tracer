module Surfaces 

  where
  import Geometry3
  import Data.Monoid
  import Debug.Trace (trace)

  newtype Sphere3 = Sphere3 (Pt3 , Float)
  {- Hit records have an intersection point, a normal, and a time -}  
  newtype HitRec = HitRec (Pt3 , Vec3 , Float) deriving Show

  {- Semigroup for hitrec TODO using semigroup instead-}
  instance Monoid HitRec where
    h1@(HitRec (_,_,t1)) `mappend` h2@(HitRec (_,_,t2)) 
      | t1 < t2 = h1 
      | otherwise = h2


--  (<>) :: Monoid a => a -> a -> a
--  (<>) = mappend
--  infixr 6 <>

  data Surface = Bbox Surface Surface Shape | Leaf Shape Material
  data Shape = Sphere Pt3 Float 
               | Triangle Pt3 Pt3 Pt3 
               | Plane Pt3 Pt3 Pt3 
               | Box Pt3 Pt3 Pt3 Pt3 Pt3 Pt3 Pt3 Pt3
  type Material = ()


  intersect :: Ray3 -> Shape -> Maybe HitRec
  intersect (Ray3 (base , dir)) (Sphere center radius) = hitRec where
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
    hitRec = if discriminant <= 0 || t < 0 then Nothing else Just (HitRec (pt , n , t)) 
  intersect (Ray3 ((base_x, base_y, base_z), (g, h, i))) (Triangle pt_a pt_b pt_c) = hitRec where
    (ax, ay, az) = pt_a
    (bx, by, bz) = pt_b
    (cx, cy, cz) = pt_c

    (a, b, c) = (ax - bx, ay - by, az - bz) 
    (d, e, f) = (ax - cx, ay - cy, az - cz)
    (j, k, l) = (ax - base_x, ay - base_y, az - base_z)
    
    (ei, hf, gf, di, dh, eg) = (e*i, h*f, g*f, d*i, d*h, e*g)
    (ak, jb, jc, al, bl, kc) = (a*k, h*b, j*c, a*l, b*l, k*c)

    (ei_hf, gf_di, dh_eg) = (ei - hf, gf - di, dh - eg)
    (ak_jb, jc_al, bl_kc) = (ak - jb, jc - al, bl - kc)

    m = a*ei_hf + b*gf_di + c*dh_eg
    beta = (j*ei_hf + k*gf_di + l*dh_eg) / m
    gamma = (i*ak_jb + h*jc_al + g*bl_kc) / m

    t = -(f*ak_jb + e*jc_al + d*bl_kc) / m
    pt = add pt_a (add (multiply (subt pt_b pt_a) beta) (multiply (subt pt_c pt_a) gamma))
    n = normalize $ cross (subt pt_b pt_a) (subt pt_c pt_a)

    hitRec = if beta < 0 || beta > 1 || gamma < 0 || beta+gamma > 1 || t < 0
             then Nothing 
             else Just (HitRec (pt , n , t)) 
