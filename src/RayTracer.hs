module RayTracer where

import Geometry3
import Objects
import Surfaces
import BoundingVolumeHierarchy
import Control.Parallel.Strategies hiding (dot)


data World = World { wImgWd :: Float
                   , wImgHt :: Float
                   , wViewWd :: Float
                   , wViewHt :: Float
                   , wViewDt :: Float
                   , wAntiAliasing :: Int --sqrt of number of rays to cast
                   , wUp :: Vec3
                   , wEye :: Vec3
                   , wCamera :: (Vec3,Vec3,Vec3)
                   , wObjects :: !BVH
                   , wAmbient :: Color
                   , wLights :: [Light]
                   , wMaxDepth :: Int
                   } deriving Show
type Pt3 = Vec3

{- jittered grids, shuffled jittered grids, world -}
render :: [([(Float,Float)],[(Float,Float)])] -> World -> [Color]
render grids world = cs where 
  ps = [ (i,j) | j <- reverse [0..wImgHt world - 1] , i <- [0..wImgWd world - 1] ]
  cs = concat $ withStrategy (parBuffer 40 rdeepseq) (chunksOf 100 (map (colorPixel world) (zip ps grids)))
  --cs = withStrategy (parBuffer 50 rdeepseq) (map (colorPixel world) (zip ps grids))
  --cs = withStrategy (parListChunk (round (wImgWd world / 4)) rdeepseq) (map (colorPixel world) (zip ps grids))
  --cs = withStrategy (parBuffer 1000 rdeepseq) $ map (raytrace world (maxDepth world) . getRay world) ps
  --cs = map (colorPixel world) (zip ps grids)

colorPixel :: World -> ((Float,Float),([(Float,Float)],[(Float,Float)])) -> Color
colorPixel w ((i,j),(pts,sh_grid)) = avgColors cs where
  d = wMaxDepth w
  cs = map (\((p,q),rfts) -> raytrace w d rfts (getRay w (i+p,j+q))) (zip pts sh_grid)
{-# INLINE colorPixel #-}


{- TODO use continuations with hitBVH -}
raytrace :: World -> Int -> (Float,Float) -> Ray3 -> Color
raytrace _ 0 _ _ = Color 0 0 0
raytrace world depth rfts (Ray3 (base,dir)) = clr where
  objs = wObjects world
  ambient_c = wAmbient world
  clr = case hitBVH (Ray3 (base,dir)) objs of
          Nothing -> Color 0 0 0
          Just (HitRec (t,obj)) ->
            let pt = add base (multiply dir t)
                n = getNormal obj pt
                v = vecM negate dir
                {- Direct color - lambertian + blinn-phong from lights-}
                direct_c = getDirectColor (wLights world) objs rfts ambient_c obj pt n v
                {- Indirect color - reflection + refraction -}
                indirect_c = getIndirectColor world depth rfts dir pt n obj
            in mixColors (+) direct_c indirect_c
{-# INLINE raytrace #-}


{- requires pt be on surface of sphere, otherwise won't be normalized -}
getNormal :: Object -> Vec3 -> Vec3
getNormal (Sphere center radius _) pt = divide (subt pt center) radius
getNormal (Triangle _ _ _ tn _) _ = tn
{-# INLINE getNormal #-}
  

getDirectColor :: [Light] 
               -> BVH
               -> (Float,Float)  --random floats [0,1) to jitter on light
               -> Color
               -> Object
               -> Pt3
               -> Vec3
               -> Vec3
               -> Color
getDirectColor [] _ _ ambient_c obj _ _ _ = mixColors (*) ambient_c (diffuseColor obj)
getDirectColor (Light l_corner l_a l_b l_c:ls) objs (ra,rb) ambient_c obj pt n v
  | shadowed = getDirectColor ls objs (ra,rb) ambient_c obj pt n v
  | otherwise = mixColors (+) color $ getDirectColor ls objs (ra,rb) ambient_c obj pt n v
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

-- TODO something very odd is going on with refraction
--  1) when dir and normal are in opposite directions, most light should
--     be refracted but it seems that most of it is being reflected SOLVED
--  2) the attenuation of the material does not seem to be having effect on
--     the color
getIndirectColor :: World -> Int -> (Float,Float) --info for recursive raytrace calls
                 -> Vec3                          --ray dir
                 -> Vec3                          --point
                 -> Vec3                          --surface normal
                 -> Object
                 -> Color
getIndirectColor w depth rfts dir pt normal obj = color 
  where
    --reflected color
    refl_c = scaleColor (* reflectionIndex obj) $ 
             mixColors (*) (specularColor obj) $
             raytrace w (depth - 1) rfts (Ray3 (pt,reflect dir normal))
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
              Just t' -> let refr_c = raytrace w (depth - 1) rfts (Ray3 (pt,t'))
                             refl_c' = scaleColor (*bigR) refl_c
                             refr_c' = scaleColor (*(1-bigR)) refr_c
                         in mixColors (*) k (mixColors (+) refl_c' refr_c')
                         --in mixColors (*) (Color 0.01 99999 0.01) (mixColors (+) refl_c' refr_c')
{-# INLINE getIndirectColor #-}


{- ray dir, surface normal -> reflected dir
 - the result is already normalized
 -}
reflect :: Vec3 -> Vec3 -> Vec3
reflect dir n = subt dir (multiply n (2 * dot dir n))
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

getRay :: World -> (Float,Float) -> Ray3
getRay world (i,j) = ray where
  vWd = wViewWd world
  vHt = wViewHt world
  vDt = wViewDt world
  iWd = wImgWd world
  iHt = wImgHt world
  (u,v,w) = wCamera world
  base = wEye world
  u_world = (i+0.5)*vWd/iWd - vWd/2.0
  v_world = (j+0.5)*vHt/iHt - vHt/2.0
  w_world = -1.0*vDt
  u_dir = multiply u u_world
  v_dir = multiply v v_world
  w_dir = multiply w w_world
  dir = add w_dir $ add u_dir v_dir
  ray = Ray3 (base,normalize dir)
{-# INLINE getRay #-}

{- dimension of grid -}
getGrid :: Int -> [(Float,Float)]
getGrid n = 
  let n' = fromIntegral n
  in [ ((p + 0.5) / n', (q + 0.5) / n') | p <- [0..n'-1] , q <- [0..n'-1] ]
{-# INLINE getGrid #-}

{- dimension of grid, list of random floats [0,1] -}
getGridR :: Int -> [Float] -> [(Float,Float)]
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

inShadow :: BVH -> Vec3 -> Vec3 -> Float -> Bool
inShadow bvh pt dir max_d= case hitBVH (Ray3 (pt,dir)) bvh of
                             Nothing -> False
                             Just (HitRec (t,_)) -> t < max_d
{-# INLINE inShadow #-}


epsilon :: Float
epsilon = 0.0001

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




data Light = Light !Vec3  --a corner of the light
                   !Vec3  --first edge of light
                   !Vec3  --second edge of light
                   !Color
                   deriving (Eq,Show,Read)


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
{-# INLINABLE hits #-}

hitBVH :: Ray3 -> BVH -> Maybe HitRec
hitBVH _ Empty = Nothing
hitBVH ray (Leaf os _) = hits ray os
hitBVH ray (Node left right _) 
  | hitsL && hitsR = max (hitBVH ray left) (hitBVH ray right)
  | hitsL = hitBVH ray left
  | hitsR = hitBVH ray right
  | otherwise = Nothing
  where
    hitsL = hitsBox ray (bvhBox left)
    hitsR = hitsBox ray (bvhBox right)
{-# INLINABLE hitBVH #-}
