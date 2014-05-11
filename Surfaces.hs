module Surfaces 

  where
  import Geometry3
  import Data.Semigroup
  import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))

  data Axis = AxisX | AxisY | AxisZ deriving (Show, Eq)
  data Color = Color !Float !Float !Float deriving (Show, Eq)

  instance Monoid Color where
    (Color r g b) `mappend` (Color r' g' b') = Color (r + r') (g + g') (b + b')
    mempty = Color 0 0 0
    
    
  epsilon :: Float
  epsilon = 0.001

  {- Ambient, Diffuse, Specular, Blinn-Phong, Reflection, Refractive Index, Attenuation -}
  type Material = (Color, Color, Color, Float, Color, Float, Color)

  {- Hit records have an intersection point, a normal, and a time -}  
  data HitRec = HitRec !Pt3 !Vec3 !Float !Material deriving (Show, Eq)

  {- Define an ordering on hit records. We define the ordering in reverse
   - to deal with the fact that Nothing is always less than Just -}
  instance Ord HitRec where
    HitRec _ _ t1 _ <= HitRec _ _ t2 _ = t1 >= t2

  type Light = (Pt3, Color)

  data Surfaces = Node !Surfaces !Surfaces !Shape | Leaf !Shape | Empty
    deriving Show
  {- Todo change color to material -}
  data Shape = Sphere !Pt3 !Float !Material
             | Triangle !Pt3 !Pt3 !Pt3 !Material
             | Plane !Pt3 !Pt3 !Pt3 !Material
             | Box !Float !Float !Float !Float !Float !Float 
     deriving Show        

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
      l  = min (x-r) $ min ax $ min bx cx
      rt = max (x+r) $ max ax $ max bx cx
      b  = min (y-r) $ min ay $ min by cy
      t  = max (y+r) $ max ay $ max by cy
      n  = min (z-r) $ min az $ min bz cz
      f  = max (z+r) $ max az $ max bz cz
    (Triangle (ax,ay,az) (bx,by,bz) (cx,cy,cz) _) <>  
      (Triangle (ax',ay',az') (bx',by',bz') (cx',cy',cz') _) = Box l r b t n f where
      l = min ax $ min bx $ min cx $ min ax' $ min bx' cx'
      r = max ax $ max bx $ max cx $ max ax' $ max bx' cx'
      b = min ay $ min by $ min cy $ min ay' $ min by' cy'
      t = max ay $ max by $ max cy $ max ay' $ max by' cy'
      n = min az $ min bz $ min cz $ min az' $ min bz' cz'
      f = max az $ max bz $ max cz $ max az' $ max bz' cz'
    t@Triangle{} <> s@Sphere{} = s <> t
    (Box l rt b t n f) <> (Sphere (x,y,z) r _) = Box l' rt' b' t' n' f' where
      l'  = min l (x-r)
      rt' = max rt (x+r)
      b'  = min b (y-r)
      t'  = max t (y+r)
      n'  = min n (z-r)
      f'  = max f (z+r)
    (Box l r b t n f) <> (Triangle (ax,ay,az) (bx,by,bz) (cx,cy,cz) _) = Box l' r' b' t' n' f' where
      l' = min l $ min ax $ min bx cx
      r' = max r $ max ax $ max bx cx
      b' = min b $ min ay $ min by cy
      t' = max t $ max ay $ max by cy
      n' = min n $ min az $ min bz cz
      f' = max f $ max az $ max bz cz
    t@Triangle{} <> b@Box{} = b <> t
    s@Sphere{} <> b@Box{} = b <> s
    _ <> _ = error "attempted to make bounding box around plane or box"

  toNonEmpty :: [a] -> NonEmpty a
  toNonEmpty [] = error "list is empty"
  toNonEmpty (a:as) = a:|as

  makeBbt :: [Shape] -> Axis -> Surfaces
  makeBbt [] _ = Empty
  makeBbt (s:[]) _ = Leaf s 
  makeBbt shapes axis = Node (makeBbt left axis') (makeBbt right axis') bbox where
    (left,right) = partition' shapes mid axis
    mid = foldr ((+) . flip getMid axis) 0.0 shapes / fromIntegral (length shapes)
    bbox = sconcat $ toNonEmpty shapes
    axis' | axis == AxisX = AxisY | axis == AxisY = AxisZ | otherwise = AxisX
  
  partition' :: [Shape] -> Float -> Axis -> ([Shape],[Shape])
  partition' ss mid axis = f ss 0.001 ([],[]) where
    f :: [Shape] -> Float -> ([Shape],[Shape]) -> ([Shape],[Shape]) 
    f [] _ ps = ps
    f (x:xs) jit (as,bs)
      | m <= mid = f xs jit' (x:as,bs)
      | otherwise = f xs jit' (as,x:bs) where
       jit' = -1*jit
       m = jit + getMid x axis
  getMid :: Shape -> Axis -> Float
  getMid (Sphere (x,_,_) _ _) AxisX = x 
  getMid (Sphere (_,y,_) _ _) AxisY = y 
  getMid (Sphere (_,_,z) _ _) AxisZ = z 
  getMid (Triangle (ax,_,_) (bx,_,_) (cx,_,_) _ ) AxisX = (ax+bx+cx) / 3
  getMid (Triangle (_,ay,_) (_,by,_) (_,cy,_) _ ) AxisY = (ay+by+cy) / 3
  getMid (Triangle (_,_,az) (_,_,bz) (_,_,cz) _ ) AxisZ = (az+bz+cz) / 3
  getMid (Box l r _ _ _ _) AxisX = (l+r) / 2
  getMid (Box _ _ b t _ _) AxisY = (b+t) / 2
  getMid (Box _ _ _ _ n f) AxisZ = (n+f) / 2
  getMid _ _ = error "Cannot calculate midpoint of this surface type"


  hits :: Ray3 -> Surfaces -> Maybe HitRec
  hits _ Empty = Nothing
  hits ray (Leaf shape) = ray `intersect` shape 
  hits ray (Node left right bbox) = 
    case ray `intersect` bbox of
      Nothing -> Nothing
      _ -> max (hits ray left) (hits ray right)

  planeHits :: Ray3 -> [Shape] -> Maybe HitRec
  planeHits _ [] = Nothing
  planeHits ray (p:planes) = max (ray `intersect` p) $ planeHits ray planes

  -- Surface intersection functions
  intersect :: Ray3 -> Shape -> Maybe HitRec
  intersect (Ray3 (base , dir)) (Sphere center radius material) = hitRec where
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
    hitRec = if discriminant <= 0 || t < epsilon
               then Nothing 
               else Just (HitRec pt n t material) 
  {- Intersection with a triangle -}
  intersect (Ray3 (base, dir)) (Triangle ta tb tc material) = hitRec where
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
    t = -(f*ak_jb + e*jc_al + d*bl_kc) / m 
    pt = add base $ multiply dir t
    n = normalize $ cross (subt tb ta) (subt tc ta)

    hitRec = if gamma < 0 || gamma > 1 || beta < 0 || beta + gamma > 1 || t < epsilon-- beta < 0 || beta > 1 || gamma < 0 || beta+gamma > 1-- || t < 0
             then Nothing 
             else Just (HitRec pt n t material) 
  {- Intersection with a plane -}
  intersect (Ray3 (base, dir)) (Plane a b c material) = hitRec where
    v1 = subt b a
    v2 = subt c a
    n = normalize $ cross v1 v2
    
    a_e = subt a base

    t = dot a_e n / dot dir n
    pt = add (multiply dir t) base

    hitRec = if t < epsilon
             then Nothing
             else Just (HitRec pt n t material)
  {- Intersection with a box -}
  intersect (Ray3 ((bx,by,bz),(dx,dy,dz))) (Box l r b t n f) 
    | t_x0 > t_y1 || t_x0 > t_z1 || t_x1 < t_y0 || t_x1 < t_z0 ||
      t_y0 > t_z1 || t_y1 < t_z0  = Nothing
    | otherwise = Just (HitRec (0,0,0) (0,0,0) 0 material)  where
    (t_x0,t_x1) = if dx >= 0 then ((l - bx) / dx , (r - bx) / dx) 
                             else ((r - bx) / dx , (l - bx) / dx)
    (t_y0,t_y1) = if dy >= 0 then ((b - by) / dy , (t - by) / dy)
                             else ((t - by) / dy , (b - by) / dy)
    (t_z0,t_z1) = if dz >= 0 then ((n - bz) / dz , (f - bz) / dz)
                             else ((f - bz) / dz , (n - bz) / dz)
    material = error "material undefined"

  isPlane :: Shape -> Bool
  isPlane Plane{} = True
  isPlane _ = False
