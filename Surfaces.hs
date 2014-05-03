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
               | Box Pt3 Pt3 Pt3 Pt3 Pt3 Pt3


  intersect :: Ray3 -> Surface -> Maybe HitRec
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
  intersect (Ray3 ((bx,by,bz),(dx,dy,dz))) (Box l r b t n f) 
    | t_x0 > t_y1 || t_x0 > t_z1 || t_x1 < t_y0 || t_x1 < t_z0 ||
      t_y0 > t_z1 || t_y1 < t_z0  = Nothing
    | otherwise = Just (HitRec ((0,0,0),(0,0,0),0))  where
    (t_x0,t_x1) = if dx >= 0 then ((l - bx) / dx , (r - bx) / dx) 
                             else ((r - bx) / dx , (l - bx) / dx)
    (t_y0,t_y1) = if dy >= 0 then ((b - by) / dy , (t - by) / dy)
                             else ((t - by) / dy , (b - by) / dy)
    (t_z0,t_z1) = if dz >= 0 then ((n - bz) / dz , (n - bz) / dz)
                             else ((f - bz) / dz , (f - bz) / dz)


