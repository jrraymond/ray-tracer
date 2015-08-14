{-# LANGUAGE BangPatterns #-}
module RayTracer where

import Geometry3
import Surfaces
import BoundingVolumeHierarchy
import Types

import Control.Arrow (first)
import GHC.Float (double2Float)
import System.Random (next)
import System.Random.Mersenne.Pure64 (PureMT, randomDouble)
import System.Random.Shuffle (shuffle')



{- jittered grids, world -}
render :: PureMT -> World -> [Color]
render rng w = cs where
  grids = generateGrids rng (round (wImgWd w) + 10) (wAntiAliasing w)
  ps = [ (i,j) | j <- reverse [0..wImgHt w - 1] , i <- [0..wImgWd w - 1] ]
  cs = map (colorPixel w) (zip ps grids)
  --cs = withStrategy (parBuffer 1000 rdeepseq) $ map (raytrace world (maxDepth world) . getRay world) ps
  --cs = map (colorPacket world . getRayPacket world) (zip ps grids)
    
--TODO clean up zipping and unzipping
colorPixel :: World -> (Point,[F6]) -> Color
colorPixel w ((i,j),grids) = avgColors cs where
  d = wMaxDepth w
  cs = map (\(F6 p q r0 r1 s0 s1) -> raytrace w d s0 s1 (getRay w (i+p,j+q) r0 r1)) grids
{-# INLINE colorPixel #-}


{- TODO use continuations with hitBVH -}
raytrace :: World -> Int -> Float -> Float -> Ray3 -> Color
raytrace _ 0 _ _ _ = Color 0 0 0
raytrace world depth s0 s1 (Ray3 (base,dir)) = clr where
  objs = wObjects world
  ambient_c = wAmbient world
  clr = case hitBVH (Ray3 (base,dir)) objs of
          Nothing -> Color 0 0 0
          Just (HitRec (t,obj)) ->
            let pt = add base (multiply dir t)
                n = getNormal obj pt
                v = vecM negate dir
                {- Direct color - lambertian + blinn-phong from lights-}
                direct_c = getDirectColor (wLights world) objs s0 s1 ambient_c obj pt n v
                {- Indirect color - reflection + refraction -}
                indirect_c = getIndirectColor world depth s0 s1 dir pt n obj
            in mixColors (+) direct_c indirect_c
{-# INLINE raytrace #-}


{- requires pt be on surface of sphere, otherwise won't be normalized -}
getNormal :: Object -> Vec3 -> Vec3
getNormal (Sphere center radius _) pt = divide (subt pt center) radius
getNormal (Triangle _ _ _ tn _) _ = tn
{-# INLINE getNormal #-}
  

getDirectColor :: [Light] 
               -> BVH
               -> Float
               -> Float  --random floats [0,1) to jitter on light
               -> Color
               -> Object
               -> Point3
               -> Vec3
               -> Vec3
               -> Color
getDirectColor [] _ _ _ ambient_c obj _ _ _ = mixColors (*) ambient_c (diffuseColor obj)
getDirectColor (Light l_corner l_a l_b l_c:ls) objs ra rb ambient_c obj pt n v
  | shadowed = getDirectColor ls objs ra rb ambient_c obj pt n v
  | otherwise = mixColors (+) color $ getDirectColor ls objs ra rb ambient_c obj pt n v
  where
    l_pt = add l_corner (add (vecM (*ra) l_a) (vecM (*rb) l_b))
    l = subt l_pt pt
    l_dir = normalize l
    shadowed = inShadow objs pt l_dir (magnitude l)
    s = max 0 $ dot l_dir n
    diffuse_c = scaleColor (*s) $ mixColors (*) (diffuseColor obj) l_c
    h = normalize $ add v l_dir
    p = phongExp obj
    spec_c = scaleColor (* (max 0 (dot n h) ** p)) $ mixColors (*) l_c (specularColor obj)
    color = mixColors (+) diffuse_c spec_c
{-# INLINE getDirectColor #-}  


getIndirectColor :: World
                 -> Int
                 -> Float
                 -> Float --info for recursive raytrace calls
                 -> Vec3                          --ray dir
                 -> Vec3                          --point
                 -> Vec3                          --surface normal
                 -> Object
                 -> Color
getIndirectColor w depth s0 s1 dir pt normal obj
  | nt == 0 = refl_c
  | otherwise = color 
  where
    --reflected color, if glossy randomly sample square to jitter reflected ray
    refl0 = reflect dir normal
    (u,v) = orthonormal refl0
    a = 1 / phongExp obj
    ju = -a / 2 + s0 * a --cheat and use shadow ray random floats
    jv = -a / 2 + s1 * a
    refl_v = add refl0 (add (multiply u ju) (multiply v jv))
    refl_c = scaleColor (* reflectionIndex obj) $ 
             mixColors (*) (specularColor obj) $ 
             raytrace w (depth - 1) s0 s1 (Ray3 (pt,refl_v))
    nt = refractionIndex obj
    --c: angle of incidence, k: attenuation, t: refracted ray
    dn = dot dir normal
    (c,k,t) 
      | dn < 0 = let c' = negate dn
                     k' = Color 1 1 1
                     t'= refract dir normal nt
                 in (c',k',t')
      | otherwise = let t' = refract dir (multiply normal (-1)) (1 / nt)
                        k' = scaleColor (exp . negate) (attenuation obj)
                        c' = case t' of
                               Nothing -> 0
                               Just t'' -> dot t'' normal
                    in (c',k',t')
    -- Schlick approximation of Fresnel equations
    r_0 = (nt - 1) * (nt - 1) / ((nt + 1) * (nt + 1))
    bigR = r_0 + (1 - r_0) * ((1 - c) ^ (5 :: Int))
    -- Beer's law
    color = case t of
              Nothing -> mixColors (*) k refl_c
              Just t' -> let refr_c = raytrace w (depth - 1) s0 s1 (Ray3 (pt,t'))
                             refl_c' = scaleColor (*bigR) refl_c
                             refr_c' = scaleColor (*(1-bigR)) refr_c
                         in mixColors (*) k (mixColors (+) refl_c' refr_c')
{-# INLINE getIndirectColor #-}


{- ray dir, surface normal -> reflected dir
 - the result is already normalized
 -}
reflect :: Vec3 -> Vec3 -> Vec3
reflect !dir !n = subt dir (multiply n (2 * dot dir n))
{-# INLINE reflect #-}

{- ray dir, normal, refraction index, maybe refracted dir 
 - assumes everything is in air
 -}
refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3
refract dir normal nt
  | x < 0 = Nothing
  | otherwise = Just t
  where
    n = 1 -- TODO everything is in air
    dn = dot dir normal
    x = 1 - n * n * (1 - dn * dn) / nt / nt
    y = multiply (subt dir (multiply normal dn)) (n / nt)
    t = normalize $ subt y (multiply normal (sqrt x))
{-# INLINE refract #-}


{- world, point on view plane, base perturbations -}
getRay :: World -> Point -> Float -> Float -> Ray3
getRay world (i,j) r1 r2 = ray where
  lens = wLens world
  eye = wEye world
  (u,v,w) = wCamera world
  ru = subt (vecM (* (r1 * lens)) u) (vecM (* (lens / 2)) u)
  rv = subt (vecM (* (r2 * lens)) v) (vecM (* (lens / 2)) v)
  base = add eye (add ru rv)
  vWd = wViewWd world
  vHt = wViewHt world
  vDt = wViewDt world
  iWd = wImgWd world
  iHt = wImgHt world
  u_world = i * vWd / iWd - vWd / 2.0
  v_world = j * vHt / iHt - vHt / 2.0
  w_world = negate vDt
  u_dir = multiply u u_world
  v_dir = multiply v v_world
  w_dir = multiply w w_world
  dir = add w_dir (add u_dir v_dir)
  fdir = subt (add dir eye) base
  ray = Ray3 (base,normalize fdir)
{-# INLINE getRay #-}

{- dimension of grid -}
getGrid :: Int -> Grid
getGrid n = 
  let n' = fromIntegral n
  in [ ((p + 0.5) / n', (q + 0.5) / n') | p <- [0..n'-1] , q <- [0..n'-1] ]
{-# INLINE getGrid #-}

{- dimension of grid, list of random floats [0,1] -}
getGridR :: Int -> [Float] -> Grid
getGridR n rs = pts where
  nn = n * n
  (irs,jrs) = splitAt nn rs
  n' = fromIntegral n
  grid = [ (p,q) | p <- [0..n'-1], q <- [0..n'-1] ]
  pts = zipWith (\(p,q) (ir,jr) -> ((p+ir)/n',(q+jr)/n')) grid (zip irs jrs)
{-# INLINE getGridR #-}

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go 
  where go xs = case splitAt n xs of
                  (ys,zs) | null ys -> []
                          | otherwise -> ys : go zs
{-# INLINE chunksOf #-}

inShadow :: BVH -> Vec3 -> Vec3 -> Float -> Bool
inShadow bvh pt dir max_d= case hitBVH (Ray3 (pt,dir)) bvh of
                             Nothing -> False
                             Just (HitRec (t,_)) -> t < max_d
{-# INLINE inShadow #-}


epsilon :: Float
epsilon = 0.00000001

diffuseColor :: Object -> Color
diffuseColor (Sphere _ _ (Material diff_c _ _ _ _ _)) = diff_c
diffuseColor (Triangle _ _ _ _ (Material diff_c _ _ _ _ _)) = diff_c
{-# INLINE diffuseColor #-}

specularColor :: Object -> Color
specularColor (Sphere _ _ (Material _ spec_c _ _ _ _)) = spec_c
specularColor (Triangle _ _  _ _ (Material _ spec_c _ _ _ _)) = spec_c
{-# INLINE specularColor #-}

phongExp :: Object -> Float
phongExp (Sphere _ _ (Material _ _ p _ _ _)) = p
phongExp (Triangle _ _ _ _ (Material _ _ p _ _ _)) = p
{-# INLINE phongExp #-}

reflectionIndex :: Object -> Float
reflectionIndex (Sphere _ _ (Material _ _ _ r _ _)) = r
reflectionIndex (Triangle _ _ _ _ (Material _ _ _ r _ _)) = r
{-# INLINE reflectionIndex #-}

refractionIndex :: Object -> Float
refractionIndex (Sphere _ _ (Material _ _ _ _ ri _)) = ri
refractionIndex (Triangle _ _ _ _ (Material _ _ _ _ ri _)) = ri
{-# INLINE refractionIndex #-}

attenuation :: Object -> Color
attenuation (Sphere _ _ (Material _ _ _ _ _ c)) = c
attenuation (Triangle _ _ _ _ (Material _ _ _ _ _ c)) = c
{-# INLINE attenuation #-}


{- triangle calculation using barycentric coordinates and projection method
- from http://www.sci.utah.edu/~wald/PhD/wald_phd.pdf
- TODO many of these can be precomputed
- as of now benchmarks show no significant difference
- AND is not working, lots of non-integers showing up in image
- -}
hit2 :: Vec3 -> Vec3 -> Object -> Maybe Float
hit2 base dir (Sphere center radius _) = 
  let ec = subt base center
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
  in if discriminant > 0 && t > epsilon then Just t
                                        else Nothing
hit2 base dir (Triangle ta tb tc tn _) 
  | dist < epsilon || beta < 0 || gamma < 0 || beta + gamma > 1 = Nothing
  | otherwise = Just dist
  where
    b = subt tc ta
    c = subt tb ta
    n@(Vec3 nx ny nz) = cross c b
    dist = negate (dot (subt base ta) n) / dot dir n
    axis | abs nx > abs ny && abs nx > abs nz = XAxis
         | abs ny > abs nx && abs ny > abs nz = YAxis
         | otherwise = ZAxis
    uaxis = nextAxis axis 1
    vaxis = nextAxis axis 2
    pu = axisV base uaxis + dist * axisV dir uaxis - axisV ta uaxis
    pv = axisV base vaxis + dist * axisV dir vaxis - axisV ta vaxis
    bu = axisV b uaxis
    bv = axisV b vaxis
    cu = axisV c uaxis
    cv = axisV c vaxis
    beta = (bu * pv - bv * pu) / (bu * cv - bv * cu)
    gamma = (cv * pu - cu * pv) / (bu * cv - bv * cu)
{-# INLINE hit2 #-}

axisV :: Vec3 -> Axis -> Float
axisV (Vec3 x y z) axis = case axis of
                            XAxis -> x
                            YAxis -> y
                            ZAxis -> z
{-# INLINE axisV #-}
  
nextAxis :: Axis -> Int -> Axis
nextAxis axis i = case (axis,i) of
                 (XAxis,1) -> YAxis
                 (XAxis,2) -> ZAxis
                 (YAxis,1) -> ZAxis
                 (YAxis,2) -> XAxis
                 (ZAxis,1) -> XAxis
                 (ZAxis,2) -> YAxis
                 (a,0)     -> a
                 (_,_)     -> error "next out of bounds"
{-# INLINE nextAxis #-}
  

{- faster than distance, I think because it it because dot products are
- faster than add/subt becuase no need to rebox -}
hit :: Vec3 -> Vec3 -> Object -> Maybe Float
hit base dir (Sphere center radius _) = mdist where
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
  mdist = if discriminant > 0 && t > epsilon
              then Just t
              else Nothing
hit (Vec3 rbx rby rbz) dir (Triangle ta tb tc tn _) = mdist where
  Vec3 ax ay az = ta
  Vec3 bx by bz = tb
  Vec3 cx cy cz = tc
  Vec3 g h i = dir

  theta = dot dir tn
                                                                            
  (a, b, c) = (ax - bx, ay - by, az - bz)                                     
  (d, e, f) = (ax - cx, ay - cy, az - cz)                                     
  (j, k, l) = (ax - rbx, ay - rby, az - rbz)                         
                                                                              
  (ei, hf, gf, di, dh, eg) = (e*i, h*f, g*f, d*i, d*h, e*g)                   
  (ak, jb, jc, al, bl, kc) = (a*k, j*b, j*c, a*l, b*l, k*c)                   
                                                                              
  (ei_hf, gf_di, dh_eg) = (ei - hf, gf - di, dh - eg)                         
  (ak_jb, jc_al, bl_kc) = (ak - jb, jc - al, bl - kc)                         
                                                                              
  m = a*ei_hf + b*gf_di + c*dh_eg                                             
  beta = (j*ei_hf + k*gf_di + l*dh_eg) / m                                    
  gamma = (i*ak_jb + h*jc_al + g*bl_kc) / m     
                                                                              
  t = -(f*ak_jb + e*jc_al + d*bl_kc) / m                                      
  mdist = if theta >= 0 || gamma < 0 || gamma > 1 || 
             beta < 0 || beta + gamma > 1 || t < epsilon
              then Nothing
              else Just t
{-# INLINE hit #-}

newtype HitRec = HitRec (Float,Object) deriving Show
instance Eq HitRec where
    HitRec (t1,_) == HitRec (t2,_) = t1 == t2
instance Ord HitRec where
    HitRec (t1,_) `compare` HitRec (t2,_) = compare t2 t1

hits :: Ray3 -> [Object] -> Maybe HitRec
hits (Ray3 (base,dir)) = go0
  where
    go0 [] = Nothing
    go0 (o:os) = case hit base dir o of
                    Nothing -> go0 os
                    Just t -> go1 t o os
    go1 t obj [] = Just (HitRec (t,obj))
    go1 t obj (o:os) = case hit base dir o of
                         Nothing -> go1 t obj os
                         Just t' 
                            | t' < t    -> go1 t' o os
                            | otherwise -> go1 t obj os
{-# INLINE hits #-}

hitBVH :: Ray3 -> BVH -> Maybe HitRec
hitBVH _ Empty = Nothing
hitBVH ray (Leaf os box) 
  | hitsBox ray box = hits ray os
  | otherwise = Nothing
hitBVH ray (Node left right box) 
  | hitsBox ray box = max (hitBVH ray left) (hitBVH ray right)
  | otherwise = Nothing
{-# INLINE hitBVH #-}

{- a frustum culls a box when all of boxes vertices lie outside of the same
- side plane -}
frustumHitsBox :: Frustum -> Box -> Bool
frustumHitsBox fr box = bb && bt && bl && br
  where
    vs = toVertices box
    bb = any (> 0) (map (\p -> dot (fBotN fr) p - fBotOffset fr) vs)
    bt = any (> 0) (map (\p -> dot (fTopN fr) p - fTopOffset fr) vs)
    bl = any (> 0) (map (\p -> dot (fLeftN fr) p - fLeftOffset fr) vs)
    br = any (> 0) (map (\p -> dot (fRightN fr) p - fRightOffset fr) vs)
{-# INLINE frustumHitsBox #-}
    
{- naive packet traversal -}
pHitBVH :: Packet -> BVH -> [Maybe HitRec]
pHitBVH (Packet _ rays) Empty = replicate (length rays) Nothing
pHitBVH p@(Packet frustum rays) (Leaf os box)
  | frustumHitsBox frustum box = map (`hits` os) rays
  | otherwise = replicate (length rays) Nothing
pHitBVH p@(Packet frustum rays) (Node left right box)
  | frustumHitsBox frustum box = zipWith max (pHitBVH p left) (pHitBVH p right)
  | otherwise = replicate (length rays) Nothing
{-# INLINE pHitBVH #-}

{- ranged traversal of bvh by a packet -}
rangedTraverse :: Packet -> BVH -> [Maybe HitRec]
rangedTraverse = undefined

{- partition traversal of bvh -}
partitionTraversal :: Packet -> BVH -> [Maybe HitRec]
partitionTraversal = undefined
    

badColor :: Color -> Bool
badColor (Color r g b) = any isNaN [r,g,b]

badVec :: Vec3 -> Bool
badVec (Vec3 x y z) = any isNaN [x,y,z]

{- Creates orthonormal basis (u,v,w) for NORMALIZED vector w -}
orthonormal :: Vec3 -> (Vec3,Vec3)
orthonormal w = let t | w == Vec3 1 0 0 = Vec3 0 1 0
                      | otherwise = Vec3 1 0 0
                    u = normalize (cross t w)
                    v = cross w u
                in (u,v)
{-# INLINE orthonormal #-}

{- random number generator, cycle size, the dimension of the grid -}
generateGrids :: PureMT -> Int -> Int -> [[F6]]
generateGrids rng num aa = cycle grids where
  rngs = iterate (snd . System.Random.next) rng
  rrs = chunksOf (2 * aa * aa) (randomFloats rng)
  grids = take num (zipWith (\gen rs -> uncurry3 (zipWith3 ptsToF6) (getGrids aa gen rs)) rngs rrs)
  ptsToF6 (a,b) (c,d) (e,f) = F6 a b c d e f


{- n x n grid and shuffled grids for depth of field and soft shadows -}
getGrids :: Int -> PureMT -> [Float] -> (Grid,Grid,Grid)
getGrids n rng rs = (aa,dof,ss) where
  aa = getGridR n rs
  dof = shuffle' aa (n * n) rng
  ss = shuffle' dof (n * n) rng


randomFloats :: PureMT -> [Float]
randomFloats rng = let (d,rng') = first double2Float (randomDouble rng)
                   in d : randomFloats rng'
{-# INLINE randomFloats #-}

{- packets
colorPacket :: World -> (Packet,[Point]) -> Color
colorPacket w (packet@(Packet _ rays),shpts) = avgColors colors
  where
    d = wMaxDepth w
    hrecs = pHitBVH packet (wObjects w)
    colors = zipWith3 (colorRay w d) shpts rays hrecs
{-# INLINE colorPacket #-}

colorRay :: World -> Int -> Point -> Ray3 -> Maybe HitRec -> Color
colorRay w depth shpt (Ray3 (base,dir)) hrec =
  case hrec of
    Nothing -> Color 0 0 0
    Just (HitRec (t,obj)) ->
      let pt = add base (multiply dir t)
          n = getNormal obj pt
          v = vecM negate dir
          ambient_c = wAmbient w
          lights = wLights w
          objs = wObjects w
          {- Direct color - lambertian + blinn-phong from lights-}
          direct_c = getDirectColor lights objs shpt ambient_c obj pt n v
          {- Indirect color - reflection + refraction -}
          indirect_c = getIndirectColor w depth shpt dir pt n obj
      in mixColors (+) direct_c indirect_c
{-# INLINE colorRay #-}

--TODO frustum bounds for depth of field rays
{- return packet of rays and list of shadow points for soft shadows -}
getRayPacket :: World -> (Point,([[Point]],(Grid,Grid))) -> (Packet,[Point])
getRayPacket w ((i,j),(rrs,(pts,sh_grid))) = (Packet frustum rays,sh_pts)
  where
    (rays,sh_pts) = unzip rays0
    rays0 = concatMap (\(rs,(p,q),rfts) -> 
              map (\r -> (getRay w (i+p,j+q) r,rfts)) rs)
              (zip3 rrs pts sh_grid)
    Ray3 (_,lowerL) = getRay w (i,j) (0,0)
    Ray3 (_,upperL) = getRay w (i,j+1) (0,0)
    Ray3 (_,upperR) = getRay w (i+1,j+1) (0,0)
    Ray3 (_,lowerR) = getRay w (i+1,j) (0,0)
    topN = normalize (cross upperR upperL)
    leftN = normalize (cross upperL lowerL)
    botN = normalize (cross lowerL lowerR)
    rightN = normalize (cross lowerR upperR)
    tOfst = dot (wEye w) topN
    bOfst = dot (wEye w) botN
    lOfst = dot (wEye w) leftN
    rOfst = dot (wEye w) rightN
    frustum = Frustum bOfst tOfst lOfst rOfst botN topN leftN rightN
{-# INLINE getRayPacket #-}
-}

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
