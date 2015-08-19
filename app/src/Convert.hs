module Convert where

import Types
import HaObj
import Surfaces
import Geometry3
import Objects
import Data.List (mapAccumL)
import qualified Data.Vector.Unboxed as U

--TODO normal interpolation
--what to do if two vertices on the triangle are the same? This occurs
--sometimes when reading in obj files that blender exports. Right now we
--just don't include them because they won't get rendered anyway.
fromMesh :: Types.Mesh -> [Object]
fromMesh m0@(Types.Mesh vs ns ms tfs) = toObj (U.toList tfs)
  where 
    toObj [] = []
    toObj (TriFace a b c mi:xs)
      | a' == b' || b' == c' || a' == c' || not (okVec3 n') = toObj xs
      | otherwise  = Triangle a' b' c' n' m' : toObj xs
      where a' = vs U.! vVertex a
            b' = vs U.! vVertex b
            c' = vs U.! vVertex c
            n' = calcNormal a' b' c'
            -- na = ns U.! vNormal a
            m' = ms U.! mi

convertMesh :: HaObj.Mesh -> Types.Mesh
convertMesh (HaObj.Mesh vs ns _ _ fs sfs _) = 
    let vs' = U.fromList (map convertV3 vs)
        ns' = U.fromList (map convertV3 ns)
        ms' = U.fromList (map (convertMaterial . fst) (fs ++ sfs))
        fs' = U.fromList (concat
                            (snd
                            (mapAccumL
                               (\a b -> (a+1,convertFaces a (snd b)))
                                0 (fs ++ sfs))))
    in Types.Mesh vs' ns' ms' fs'

convertV3 :: V3 -> Vec3
convertV3 (V3 x y z) = Vec3 x y z

convertFaceV :: FaceV -> Vertex
convertFaceV (FaceV v t n) = Vertex (v - 1) (t - 1) (n - 1)

convertFace :: Int -> Face -> TriFace
convertFace mIx (Face (a:b:c:[])) = TriFace a' b' c' mIx
  where (a',b',c') = mapT3 convertFaceV (a,b,c)
convertFace _ (Face xs) = error ("non-triangle face " ++ show (length xs))

convertFaces :: Int -> [Face] -> [TriFace]
convertFaces mIx = map (convertFace mIx)

convertMaterial :: HaObj.Material -> Types.Material
convertMaterial m = 
  case im of 
    0  -> makeMaterial dC    xx     0 0    0 xx
    1  -> makeMaterial dC    xx     0 0    0 xx
    2  -> makeMaterial dC    sC phong 0    0 xx
    3  -> makeMaterial dC    sC phong 1    0 opaque
    4  -> makeMaterial dC    sC phong 0 refr white
    5  -> makeMaterial dC    sC phong 1    0 opaque
    6  -> makeMaterial dC    sC phong 1 refr opaque
    7  -> makeMaterial dC    sC phong 1 refr opaque
    8  -> makeMaterial dC    sC phong 1 refr opaque
    9  -> makeMaterial xx white phong 1 refr white
    10 -> makeMaterial dC sC phong 0    0 xx
    i  -> error ("invalid illum value " ++ show i)
  where
    xx = Color 0 0 0
    white = Color 1 1 1
    opaque = Color 99 99 99
    im = mIllum m
    dC = convertColor (mKd m)
    sC = convertColor (mKs m)
    phong | mNs m == 0 = 10 | otherwise = mNs m * 10
    refr = mNi m
{- 0  Color on and Ambient off 
 - 1 Color on and Ambient on 
 - 2 Highlight on 
 - 3 Reflection on and Ray trace on 
 - 4 Transparency: Glass on Reflection: Ray trace on 
 - 5 Reflection: Fresnel on and Ray trace on 
 - 6 Transparency: Refraction on Reflection: Fresnel off and Ray trace on 
 - 7 Transparency: Refraction on Reflection: Fresnel on and Ray trace on 
 - 8 Reflection on and Ray trace off 
 - 9 Transparency: Glass on Reflection: Ray trace off 
 - 10  Casts shadows onto invisible surfaces 
-}

convertColor :: V3 -> Color
convertColor (V3 r g b) = Color r g b

