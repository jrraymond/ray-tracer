module Main where

import BenchmarkScene
import BenchmarkScene2
import BenchmarkScene3
import BenchmarkScene4
import GlassCubesScene
import Geometry3
import BoundingVolumeHierarchy
import Objects
import Surfaces
import RayTracer

import Control.Arrow (first)
import GHC.Float (double2Float)
import System.Random (next)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT, randomDouble)
import System.Random.Shuffle (shuffle')
import Options.Applicative


{- 
- TODO scene file parsing
- TODO bounding boxes
- TODO depth of field
- TODO light dissapation
-}

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> configure)
                    (fullDesc <> header "Ray Tracer" )


run :: Config -> IO ()
run _ = do 
  let c = bench4Config
  let w = bench4World
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


mapT3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapT3 f (x,y,z) = (f x, f y, f z)

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
bench3World :: World
bench3World = configToWorld bench3Config bench3Objects bench3Lights

bench4Config :: Config
bench4Config = Config 800 600 
                      80 60 70
                      6
                      25
                      25
                      0.10
                      (Vec3 0 1 0)
                      (Vec3 50 5 0)
                      (Vec3 0 0 0)
bench4World :: World
bench4World = configToWorld bench4Config bench4Objects bench4Lights

glassConfig :: Config
glassConfig = Config 800 600
                     8 6 7  
                     10 
                     64
                     1
                     0
                     (Vec3 0 1 0)
                     (Vec3 20 0 0)
                     (Vec3 0 0 0)

glassWorld :: World
glassWorld = configToWorld glassConfig glassObjects glassLights
