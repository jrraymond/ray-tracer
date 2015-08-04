module Main where

import BenchmarkScene
import BenchmarkScene2
import BenchmarkScene3
import BenchmarkScene4
import BenchmarkScene5
import Geometry3
import BoundingVolumeHierarchy
import Materials
import Objects
import Surfaces
import RayTracer
import HaObj

import Control.Arrow (first)
import Data.List (mapAccumL)
import qualified Data.Vector.Unboxed as U
import GHC.Float (double2Float)
import System.Random (next)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT, randomDouble)
import System.Random.Shuffle (shuffle')
import Options.Applicative

import Debug.Trace

{- 
- TODO scene file parsing
- TODO light dissapation
- TODO texture mapping
- TODO triangle meshes
- TODO glossy reflection - DONE but I think it could be improved:
-   1) I use phong exp to determine glossy reflection, is this ok?
-   2) the is another method that I'd like to try that sounds fancy and has
-   the words monte-carlo in it
-}

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> configure)
                    (fullDesc <> header "Ray Tracer" )


run :: Config -> IO ()
run _ = do 
  let c = bench1Config
  objs <- case cScene c of
            Nothing -> return []
            Just fname -> do ms <- parseObj fname
                             case ms of
                               Left e -> error (show e)
                               Right mesh -> return $ fromMesh (convertMesh mesh)
  --let w = bench6World objs
  let w = bench1World
  rng <- newPureMT
  let rs = chunksOf (wAntiAliasing w) (chunksOf (wDOF w) (randomPairs rng))
  let grids = generateGrids rng (cImageWidth c + 10) (wAntiAliasing w)
  let img = render (zip rs grids) w
  putStrLn "rendering . . ."
  writePPM "img.ppm" (cImageWidth c) (cImageHeight c) img
  putStrLn ". . . done"

{- random number generator, cycle size, the dimension of the grid -}
generateGrids :: PureMT -> Int -> Int -> [(Grid,Grid)]
generateGrids rng num aa = cycle grids where
  rngs = iterate (snd . System.Random.next) rng
  rs = randomFloats rng
  grids = take num (zipWith (getGridPair aa) rngs (chunksOf (2 * aa * aa) rs))


{- n x n grid and shuffled grid -}
getGridPair :: Int -> PureMT -> [Float] -> (Grid,Grid)
getGridPair n rng rs = (grid,shuffled) where
  grid = getGridR n rs
  shuffled = shuffle' grid (n * n) rng


randomFloats :: PureMT -> [Float]
randomFloats rng = let (d,rng') = first double2Float (randomDouble rng)
                   in d : randomFloats rng'

randomPairs :: PureMT -> [(Float,Float)]
randomPairs rng = let (a,rng') = first double2Float (randomDouble rng)
                      (b,rng'') = first double2Float (randomDouble rng')
                  in (a,b) : randomPairs rng''


data Config = Config { cImageWidth :: Int
                     , cImageHeight :: Int
                     , cViewWidth :: Int
                     , cViewHeight :: Int
                     , cViewDistance :: Int
                     , cReflectionDepth :: Int
                     , cAntiAliasing :: Int
                     , cDOF :: Int
                     , cLens :: Float
                     , cUp :: Vec3
                     , cEye :: Vec3
                     , cLookAt :: Vec3
                     , cScene :: Maybe String
                     } deriving (Eq,Read,Show)

configure :: Parser Config
configure = Config <$> option auto (long "width" <> value 400 <>
                                   help "image width in pixels, default 400")
                   <*> option auto (long "height" <> value 300 <>
                                   help "image height in pixels, default 300")
                   <*> option auto (long "view-width" <> value 4 <>
                                   help "width of viewport, default 4")
                   <*> option auto (long "view-height" <> value 3 <>
                                   help "height of viewport, default 3")
                   <*> option auto (long "view-distance" <> value 7 <>
                                   help "distance to viewport, default 7")
                   <*> option auto (long "reflection-depth" <> value 3 <>
                                   help "maximum reflections, default 3")
                   <*> option auto (long "anti-aliasing" <> value 1 <>
                                   help "rays per pixel, default 1")
                   <*> option auto (long "depth-of-field" <> value 1 <>
                                   help "depth of field rays, default 1")
                   <*> option auto (long "lens" <> value 0 <>
                                   help "lens size, default 0")
                   <*> option auto (long "up" <> metavar "UP_DIRECTION" <>
                                   value (Vec3 0 1 0) <>
                                   help "unit vector indicating up, default 0 1 0")
                   <*> option auto (long "eye" <> metavar "EYE_POSITION" <>
                                   value (Vec3 0 0 0) <>
                                   help "initial position of the eye, default 0 0 0")
                   <*> option auto (long "look-at" <> metavar "LOOK_AT_POINT" <>
                                   value (Vec3 1 0 0) <>
                                   help "initial point to look at, default 1 0 0")
                   <*> optional (strOption (long "scene" <>
                                           metavar "SCENE_FILE" <>
                                           help ".obj file"))



{- Some of our pixel floats are outside the range [0,1], GLUT clamps 
- - all float in the range [0,1], but writing a ppm does not, so we need
- - this to write the ppm -}
clamp :: Float -> Float
clamp x | x < 0 = 0 | x > 1 = 1 | otherwise = x
{-# INLINE clamp #-}


configToWorld :: Config -> [Object] -> [Light] -> World
configToWorld c objs lights = 
    World { wImgWd = fromIntegral (cImageWidth c)
          , wImgHt = fromIntegral (cImageHeight c)
          , wViewWd = fromIntegral (cViewWidth c)
          , wViewHt = fromIntegral (cViewHeight c)
          , wViewDt = fromIntegral (cViewDistance c)
          , wAntiAliasing = floor (sqrt (fromIntegral (cAntiAliasing c) :: Float))
          , wDOF = cDOF c
          , wLens = cLens c
          , wUp = cUp c
          , wEye = cEye c
          , wCamera = getCam (cEye c) (cLookAt c) (cUp c)
          , wObjects = sahBVH objs
          , wAmbient = Color 0.1 0.1 0.1
          , wLights = lights
          , wMaxDepth = cReflectionDepth c
          }

--TODO normal interpolation
--what to do if two vertices on the triangle are the same? This occurs
--sometimes when reading in obj files that blender exports. Right now we
--just don't include them because they won't get rendered anyway.
fromMesh :: Objects.Mesh -> [Object]
fromMesh m0@(Objects.Mesh vs ns ms tfs) = toObj (U.toList tfs)
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

convertMesh :: HaObj.Mesh -> Objects.Mesh
convertMesh (HaObj.Mesh vs ns _ _ fs sfs _) = 
    let vs' = U.fromList (map convertV3 vs)
        ns' = U.fromList (map convertV3 ns)
        ms' = U.fromList (map (convertMaterial . fst) (fs ++ sfs))
        fs' = U.fromList (concat
                            (snd
                            (mapAccumL
                               (\a b -> (a+1,convertFaces a (snd b)))
                                0 (fs ++ sfs))))
    in Objects.Mesh vs' ns' ms' fs'

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

convertMaterial :: HaObj.Material -> Surfaces.Material
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

getCam :: Vec3 -> Vec3 -> Vec3 -> (Vec3,Vec3,Vec3)
getCam e lat up = (u,v,w) where
  w = normalize $ e `subt` lat
  u = normalize $ up `cross` w
  v =  w `cross` u
  
writePPM :: String -> Int -> Int -> [Color] -> IO ()
writePPM name w h pixels = writeFile name txt  where
  toTxt :: Float -> String
  toTxt = (++ " ") . show . (truncate :: Float -> Int) . (255*) . clamp
  {-# INLINE toTxt #-}
  f :: [Color] -> String
  f [] = ""
  f (Color r g b:ps) = toTxt r ++ toTxt g ++ toTxt b ++ f ps
  {-# INLINE f #-}
  txt = "P3\n" ++ show w ++ " " ++ show h ++
        " 255\n" ++ f pixels

{- for using the repl -}
scene1C :: Config
scene1C = Config 800 600 8 6 7 6 25 1 0 (Vec3 0 1 0) (Vec3 10 0 0) (Vec3 0 0 0) Nothing
scene1W :: World
scene1W = configToWorld scene1C [ Sphere (Vec3 0 0 0) 1 redM ] bench1Lights
{-
rng_ = newPureMT
rs_ = chunksOf (wAntiAliasing scene1W) (chunksOf (wDOF scene1W) (randomPairs rng_))
grids_ = generateGrids rng_ (cImageWidth scene1C + 10) (wAntiAliasing scene1W)
randoms_ = zip rs_ grids_
-}
bench1Config :: Config
bench1Config = Config 800 600
                     8 6 7
                     6
                     25
                     1
                     0
                     (Vec3 0 1 0)
                     (Vec3 20 5 20)
                     (Vec3 0 0 0)
                     Nothing

bench1World :: World
bench1World = configToWorld bench1Config bench1Objects bench1Lights


bench2Config :: Config
bench2Config = Config 800 600 
                     8 6 7
                     6
                     25
                     1
                     0
                     (Vec3 0 1 0)
                     (Vec3 25 10 25)
                     (Vec3 0 0 0)
                     Nothing

bench2World :: World
bench2World = configToWorld bench2Config bench2Objects bench2Lights

bench3Config :: Config
bench3Config = Config 800 600 
                     8 6 7
                     6
                     25
                     1
                     0
                     (Vec3 0 1 0)
                     (Vec3 25 0 25)
                     (Vec3 0 0 0)
                     Nothing

bench3World :: World
bench3World = configToWorld bench3Config bench3Objects bench3Lights

--depth of field example
bench4Config :: Config
bench4Config = Config 800 600 
                      8 6 7
                      6
                      64
                      25
                      0.10
                      (Vec3 0 1 0)
                      (Vec3 50 5 0)
                      (Vec3 0 0 0)
                      Nothing

bench4World :: World
bench4World = configToWorld bench4Config bench4Objects bench4Lights

--glossy example
bench5Config :: Config
bench5Config = Config 800 600 
                      8 6 7
                      6
                      64
                      1
                      0.0
                      (Vec3 0 1 0)
                      (Vec3 20 0 0)
                      (Vec3 0 0 0)
                      Nothing

bench5World :: World
bench5World = configToWorld bench5Config bench5Objects bench5Lights

--scene parsing example
bench6Config :: Config
bench6Config = Config 800 600 
                      8 6 8
                      6
                      64
                      1
                      0.0
                      (Vec3 0 1 0)
                      (Vec3 5 2 5)
                      --(Vec3 18 12 18)
                      (Vec3 0 0 0)
                      (Just "br-ship.obj")

bench6World :: [Object] -> World
bench6World objs = configToWorld bench6Config objs
                    [Light (Vec3 10 20 0) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 (-10) 20 0) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 0 20 10) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 0 20 (-10)) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 0 (-20) 10) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 0 (-20) (-10)) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 10 (-20) 0) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)
                    ,Light (Vec3 (-10) (-20) 0) (Vec3 0 0 0) (Vec3 0 0 0) (Color 0.5 0.5 0.5)]
