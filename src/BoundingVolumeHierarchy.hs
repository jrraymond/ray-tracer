module BoundingVolumeHierarchy where

import Objects
import Geometry3
import Data.List (foldl1',sortOn,scanl',minimumBy,maximumBy)
import Data.Ord (comparing)

data Axis = XAxis | YAxis | ZAxis deriving (Eq,Read,Show)

{- Bounding Box -}
data Box = Box { boxXmin  :: !Float
               , boxXmax  :: !Float
               , boxYmin  :: !Float
               , boxYmax  :: !Float
               , boxZmin  :: !Float
               , boxZmax  :: !Float }
         | EmptyBox deriving (Eq,Show,Read)


{- Bounding Volume Hierarchy -}
data BVH = Node !BVH !BVH !Box 
         | Leaf ![Object] !Box
         | Empty
         deriving (Eq,Show,Read)

bvhBox :: BVH -> Box
bvhBox (Node _ _ b) = b
bvhBox (Leaf _ b) = b
bvhBox Empty = error "bvh: no box"
{-# INLINABLE bvhBox #-}

printBVH :: BVH -> String 
printBVH bvh = go bvh 0 
  where 
    go (Node bvh1 bvh2 box) n = replicate n ' ' ++ 
                                "Node [" ++ show box ++ "]\n" ++
                                 go bvh1 (n+1) ++ "\n" ++ go bvh2 (n+1)
    go (Leaf os b) n = replicate n ' ' ++ "Leaf [" ++ show (length os) ++ "] "++ show b
    go Empty n = replicate n ' ' ++ "Empty"

{- Top down creation of bounding volume hierarchy by surface area heuristic
- as described in "Ray Tracing Deformable Scenes Using Dynamic Bounding
- Volume Hierarchies"
-}
sahBVH :: [Object] -> BVH
sahBVH objs = go (map (\o -> (toBox o,o)) objs) where
  go :: [(Box,Object)] -> BVH
  go xs 
    | bestCost > leafCost = Leaf (map snd xs) leafBox
    | otherwise = Node left right nodeBox
    where
      t_tri = 1.0
      leafCost = t_tri * fromIntegral (length xs)
      boxes = map fst xs
      bests = map (\a -> (a,bestSplit boxes a)) [XAxis,YAxis,ZAxis]
      (bestAxis,(bestIx,bestCost)) = minimumBy (comparing (snd . snd)) bests
      (ls,rs) = splitAt bestIx (sortOn (midBox bestAxis . fst) xs)
      left = go ls
      right = go rs
      nodeBox = surround (bvhBox left) (bvhBox right)
      leafBox = foldl1' surround boxes

{- uses SAH to find a good split of list of boxes along a specific axis
- returns the index of the split and estimated cost of split -}
bestSplit :: [Box] -> Axis -> (Int,Float)
bestSplit [_] _ = (0,inf)
bestSplit boxes0 axis = 
    let n = length boxes
        boxes = sortOn (midBox axis) boxes0
        box = foldl1' surround boxes
        isa = 1 / surfaceArea box
        lbs = scanl' surround EmptyBox boxes
        rbs = scanr surround EmptyBox boxes
        --box = head rbs
        costs = zipWith3 (\lb rb i -> let lsa = surfaceArea lb
                                          rsa = surfaceArea rb
                                      in (i,sahCost isa i lsa (n - i) rsa))
                         lbs rbs [0..]
    in minimumBy (comparing snd) (filter (not . isNaN . snd) costs)



{- The cost function is
-   T = 2T_AABB + A(S_1)/A(S)*N(S_1)*T_tri + A(S_2)/A(S)*N(S_2)*T_tri
- where A() is the area of the bounds of Set S and N is the number and
- T_AABB and T_tri are costs of intersections with box and triangle.
- isa -> 1 / surface area of box -}
sahCost :: Float -> Int -> Float -> Int -> Float -> Float
sahCost isa n1 sa1 n2 sa2 = cost where
  t_aabb  = 0.3 --cost of intersectioin with box
  t_tri = 1.0 --cost of intersection with triangle
  cost = 2 * t_aabb + t_tri * isa * (sa1 * n1' + sa2 * n2')
  n1' = fromIntegral n1
  n2' = fromIntegral n2

surfaceArea :: Box -> Float
surfaceArea EmptyBox = inf
surfaceArea (Box l r b t n f) = let w = abs r - l
                                    h = abs t - b
                                    d = abs f - n
                                in 2 * (w * h + w * d + h * d)

{- Top down creation of a bounding volume hierarchy by partitioning objects
- based on the centroid
- creates a leaf when the standard deviation of midpoints along the given
- axis is less than or equal to one.
- picks axis to partition objects on by the variance of midpoints along it
- -}
meanBVH :: [Object] -> BVH
meanBVH objs = go (map (\o -> (toBox o,o)) objs)
  where
    go [] = Empty
    go xs
      | n <= 2 || sqrt var <= 0.5 = Leaf (map snd xs) box
      | otherwise = Node (go left) (go right) box
      where
        boxes = map fst xs
        axes = [XAxis,YAxis,ZAxis]
        vs = zip (map (midPtVariance boxes) axes) axes
        ((var,mid),bestAxis) = maximumBy (comparing (fst . fst)) vs
        n = fromIntegral (length objs) :: Float
        (left,right) = partition' xs mid bestAxis
        box = foldl1' surround boxes

{- calculates the variance of midpoints of boxes along axis -}
midPtVariance :: [Box] -> Axis -> (Float,Float)
midPtVariance boxes axis = (variance,mid)
  where
    n = fromIntegral (length boxes)
    midPts = map (midBox axis) boxes
    mid = foldl1' (+) midPts / n
    dvtns = map (\x -> (x - mid) ** (2 :: Float)) midPts
    variance = foldl1' (+) dvtns / n



nextAxis :: Axis -> Axis
nextAxis axis = case axis of
                  XAxis -> YAxis
                  YAxis -> ZAxis
                  ZAxis -> XAxis



surround :: Box -> Box -> Box
surround EmptyBox EmptyBox = EmptyBox
surround EmptyBox b@(Box{}) = b
surround b@(Box{}) EmptyBox = b
surround (Box l0 r0 b0 t0 n0 f0)
         (Box l1 r1 b1 t1 n1 f1) = Box (min l0 l1) (max r0 r1)
                                       (min b0 b1) (max t0 t1)
                                       (min n0 n1) (max f0 f1)


{- partitions a list of boxes,objects by a point on the axis -}
partition' :: [(Box,Object)] -> Float -> Axis -> ([(Box,Object)],[(Box,Object)])
partition' xs0 mid axis = f xs0 0.00001 ([],[]) where
  f [] _ ps = ps
  f (x:xs) jitter (as,bs)
    | m <= mid = f xs jitter' (x:as,bs)
    | otherwise = f xs jitter' (as,x:bs)
    where
      jitter' = negate jitter
      m = jitter + midBox axis (fst x)


midPt :: Axis -> Object -> Float
midPt XAxis (Sphere (Vec3 x _ _) _ _) = x
midPt YAxis (Sphere (Vec3 _ y _) _ _) = y
midPt ZAxis (Sphere (Vec3 _ _ z) _ _) = z
midPt XAxis (Triangle (Vec3 ax _ _)
                      (Vec3 bx _ _)
                      (Vec3 cx _ _) _ _) = (ax + bx + cx) / 3
midPt YAxis (Triangle (Vec3 _ ay _)
                      (Vec3 _ by _)
                      (Vec3 _ cy _) _ _) = (ay + by + cy) / 3
midPt ZAxis (Triangle (Vec3 _ _ az)
                      (Vec3 _ _ bz)
                      (Vec3 _ _ cz) _ _) = (az + bz + cz) / 3

midBox :: Axis -> Box -> Float
midBox _ EmptyBox = error "empty box"
midBox XAxis (Box l r _ _ _ _) = (l + r) / 2
midBox YAxis (Box _ _ b t _ _) = (b + t) / 2
midBox ZAxis (Box _ _ _ _ n f) = (n + f) / 2


toBox :: Object -> Box
toBox (Sphere (Vec3 x y z) r _) = Box (x - r) (x + r)
                                      (y - r) (y + r)
                                      (z - r) (z + r)
toBox (Triangle (Vec3 ax ay az)
                (Vec3 bx by bz)
                (Vec3 cx cy cz) _ _) = Box (minimum [ax,bx,cx])
                                           (maximum [ax,bx,cx])
                                           (minimum [ay,by,cy])
                                           (maximum [ay,by,cy])
                                           (minimum [az,bz,cz])
                                           (maximum [az,bz,cz])

hitsBox :: Ray3 -> Box -> Bool
hitsBox _ EmptyBox = error "empty box"
hitsBox (Ray3 (Vec3 bx by bz,Vec3 dx dy dz)) (Box l r b t n f) = hits
  where
    (t_x0,t_x1) | dx >= 0   = ((l - bx) / dx , (r - bx) / dx) 
                | otherwise = ((r - bx) / dx , (l - bx) / dx)
    (t_y0,t_y1) | dy >= 0   = ((b - by) / dy , (t - by) / dy)
                | otherwise = ((t - by) / dy , (b - by) / dy)
    (t_z0,t_z1) | dz >= 0   = ((n - bz) / dz , (f - bz) / dz)
                | otherwise = ((f - bz) / dz , (n - bz) / dz)
    hits = not (t_x0 > t_y1 || t_x0 > t_z1 ||
                t_x1 < t_y0 || t_x1 < t_z0 ||
                t_y0 > t_z1 || t_y1 < t_z0)
{-# INLINABLE hitsBox #-}
