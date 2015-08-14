module Main where

import BenchmarkScene
import BenchmarkScene2
import BenchmarkScene3
import BenchmarkScene4
import BenchmarkScene5
import Geometry3
import BoundingVolumeHierarchy
import Materials
import RayTracer
import HaObj
import Types
import Convert

import System.Random.Mersenne.Pure64 (newPureMT)
import Options.Applicative

{- 
- TODO scene file parsing
- TODO light dissapation
- TODO texture mapping
- TODO triangle meshes
-}

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> configure)
                    (fullDesc <> header "Ray Tracer" )


run :: Config -> IO ()
run _ = do 
  let c = bench4Config
  objs <- case cScene c of
            Nothing -> return []
            Just fname -> do ms <- parseObj fname
                             case ms of
                               Left e -> error (show e)
                               Right mesh -> return $ fromMesh (convertMesh mesh)
  --let w = bench6World objs
  let w = bench4World
  rng <- newPureMT
  let img = render rng w
  putStrLn "rendering . . ."
  writePPM "img.ppm" (cImageWidth c) (cImageHeight c) img
  putStrLn ". . . done"


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
bench4Config = Config 800 600 --3200 1800
                      8 6 7 -- 16 9 10
                      6 --10 --6
                      25 --64
                      25 --25
                      0.1
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
                      4
                      25
                      1
                      0.0
                      (Vec3 0 1 0)
                      --(Vec3 5 2 5)
                      (Vec3 18 12 18)
                      (Vec3 0 0 0)
                      (Just "station.obj")

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
