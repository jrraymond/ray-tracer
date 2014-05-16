{- @author Brian Gapinski
-  @author Justin Raymond
-
-  Ray Tracer
-}
{-# LANGUAGE BangPatterns #-}
import Foreign
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import Surfaces
import RayTracer
import Geometry3
import Data.List (sortBy, partition)
import System.Exit
import System.Console.GetOpt
import Data.Maybe
import Parser
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import System.Cmd


main :: IO ()
main = do 
    --initialize OpenGL systems
    (_progName, _args) <- GLUT.getArgsAndInitialize

    let (opts,errs) = parseOpts _args
        iwd = fromMaybe 80 (optImgWd opts)
        iht = fromMaybe 60 (optImgHt opts)
        ciwd = fromIntegral iwd
        ciht = fromIntegral iht
        as = fromMaybe 0 (optAntiAliasing opts)
        ss = fromMaybe 1 (optSoftShadows opts)
        rd = fromMaybe 2 (optReflDepth opts)
        frames = fromMaybe 1 (optFrames opts)
        rng = mkStdGen $ fromIntegral frames
  

    sceneFile <- checkFile $ optScene opts
    c <- case parseScene sceneFile of
           Left err -> putStrLn (show err) >> exitWith (ExitFailure 1)
           Right r -> return r
    --colorMap <- checkSource $ readColors Map.empty colorFin 
    --print colorMap
    --matFin <- checkFile $ optMaterialF opts
    --matMap <-  checkSource $ readMaterials colorMap matFin
    --print matMap
    --shapeFin <- checkFile $ optShapeF opts
--  --  shapeMap <- checkSource $ readShapes matMap shapeFin
    --shapeExprMap <- undefined --checkSource $ readShapesExpr matMap shapeFin
    --print (getShapesNow 0 (map snd $ Map.toList shapeExprMap))

    --let (planes',shapes') = partition isPlane $ map snd (Map.toList undefined)
    --    world = World (iwd,iht) (8,6,4) (u,v,w) eye' lookAt' shapes' planes' (makeBbt shapes' AxisX) lts amb as ss rd
    --    pixels = render world
    --    display :: GLUT.DisplayCallback
    --    display = do
    --      --clears out the graphics color state
    --      GLUT.clear [ GLUT.ColorBuffer ]
    --      --(GL.Size x y) <- GLUT.get GLUT.windowSize
    --      arr <- newArray (flatten pixels) :: IO (Ptr Float)
    --      --arr <- FMU.new (VS.replicate 100 (1 :: Float))
    --      GL.drawPixels (GL.Size ciwd ciht) (PixelData GL.RGB GL.Float arr)
    --      --GL.drawPixels size undefined
    --      --pushes our OpenGL commands down to the systems graphics for display
    --      GLUT.flush
    --GLUT.initialWindowSize GLUT.$= GL.Size ciwd ciht
    ----open the main window
    --_window <- GLUT.createWindow "Ray Tracer"
    ----set the display callback for the main window
    --GLUT.displayCallback GLUT.$= display
    --GLUT.reshapeCallback GLUT.$= Just reshape
    ----let GLUT take over
    --GLUT.mainLoop

--eye' = (15, 2, 15)
--lookAt' = (-1, -1, -1)
--up = (0,1,0)
--w = normalize $ subt eye' lookAt'
--u = normalize $ cross up w
--v = cross w u
--lts = [ ((50, 20, 0), Color 0.5 0.5 0.5)
    --, ((3, 2, 20), Color 0.2 0.2 0.2)
    --]
--amb = Color 0.1 0.1 0.1
          
    --shapesExpr <- return $ map snd (Map.toList shapeExprMap)
    --writePPM "output.ppm" iwd iht $ invertY iwd iht pixels
    let vpw = cViewPlane c
        amb = cAmbient c
        go :: Float -> IO ()
        go i | i > 0 = do let eye = evalExprTuple i $ cEye c
                              lookat = evalExprTuple i $ cLookAt c
                              up = evalExprTuple i $ cUp c
                              w = normalize $ subt eye lookat
                              u = normalize $ cross up w
                              v = cross w u
                              planes = map (evalShapeExpr i) $ cPlanes c
                              shapes = map (evalShapeExpr i) $ cSurfaces c
                              lights = map (evalLightExpr i) $ cLights c
                              world = World (iwd,iht) vpw (u,v,w) eye lookat 4 planes (makeBbt shapes AxisX) lights amb as ss rd rng
                          pixels' <- return $ invertY iwd iht (render world)
                          putStrLn $ "Writing frame " ++ show i
                          writePPM ("img/output" ++ prefix frames i ++ ".ppm") iwd iht pixels'
                          go (i - 1)
             | otherwise   = putStrLn "All done!" >> return () 
    go (fromIntegral frames)
    system "convert -delay 25 img/output*.ppm img/output.gif" >> return ()

prefix :: Int -> Float -> String
prefix f i = pre ++ i' where
  i' = show i
  pre = replicate (length (show f) + 2 - length i') '0' 

reshape :: GLUT.ReshapeCallback
reshape size = GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)

writePPM :: String -> Int -> Int -> [Color] -> IO ()
writePPM name w h pixels = writeFile name string  where 
  toStr :: Float -> String
  toStr = (++ " ") . show . (truncate :: Float -> Int) . (255*) . clamp
  f :: [Color] -> String
  f [] = ""
  f (Color r g b:ps) = toStr r ++ toStr g ++ toStr b ++ f ps
  string = "P3\n" ++ show w ++ " " ++ show h ++ " 255\n" ++ f pixels

{- Flips the array of pixels to account for GLUT and ppm having different
- origins. GLUT's origin is the lower left hand corner, but ppm expects 
- the origin to be in the upper right hand corner.
-}
invertY :: Int -> Int -> [Color] -> [Color]
invertY wd ht cs = map snd $ sortBy g $ zipWith (curry f) [0..] cs where
  f :: (Int,Color) -> (Int,Color)
  f (i,v) = (wd * (ht - i `div` wd) + i `mod` wd,v)
  g :: (Int,Color) -> (Int,Color) -> Ordering
  g (i,_) (j,_) = i `compare` j

{- Some of our pixel floats are outside the range [0,1], GLUT clamps 
- all float in the range [0,1], but writing a ppm does not, so we need
- this to write the ppm -}
clamp :: Float -> Float
clamp x | x < 0 = 0 | x > 1 = 1 | otherwise = x

{- Format for command line arguments -}
usage :: String
usage = "usage raytracer [-win_wd=] [-win_ht=]"

{- Parses the command line arguments -}

data Options = Options
  { optScene :: Either String String
  , optImgWd :: Maybe Int
  , optImgHt :: Maybe Int
  , optAntiAliasing :: Maybe Int
  , optSoftShadows :: Maybe Int
  , optReflDepth :: Maybe Int
  , optFrames :: Maybe Int
  , optSeed :: Maybe Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optScene = Left "No Scene File"
  , optImgWd = Just 800
  , optImgHt = Just 600
  , optAntiAliasing = Just 1
  , optSoftShadows = Just 0
  , optReflDepth = Just 2
  , optFrames = Just 1
  , optSeed = Just 356
  }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['s'] ["scene"] (ReqArg (\x opts -> opts {optScene = Right x}) "") "Scene file"
    , Option ['w'] ["imageWidth"] (ReqArg (\w opts -> opts { optImgWd = readInt w}) "800") "image width in pixels"
    , Option ['h'] ["imageHeight"] (ReqArg (\h opts -> opts { optImgHt = readInt h}) "600") "image height in pixels"
    , Option [] ["antialiasing"] (ReqArg (\a opts -> opts { optAntiAliasing = readInt a}) "1") "antialiasing level"
    , Option [] ["softshadows"] (ReqArg (\s opts -> opts { optSoftShadows = readInt s }) "0") "shoftshadow level"
    , Option [] ["reflectiondepth"] (ReqArg (\d opts -> opts { optReflDepth = readInt d}) "2") "reflection depth"
    , Option ['f'] ["frames"] (ReqArg (\f opts -> opts { optFrames = readInt f}) "1") "frames"
    , Option [] ["seed"] (ReqArg (\s opts -> opts { optSeed = readInt s}) "356") "seed"
    ]

readInt :: String -> Maybe Int
readInt = fmap fst . listToMaybe . reads

parseOpts :: [String] -> (Options, [String])
parseOpts args =
    case getOpt Permute options args of
      (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
      (_,_,ers) -> (defaultOptions, [concat ers ++ usageInfo header options]) where header = "Usage: ./raytracer -s[--scene] <scene-file> [-w,-h,--antialiasing,--softshadows]"

checkFile :: Either String String -> IO String
checkFile (Left e) = putStrLn e >> exitWith (ExitFailure 1)
checkFile (Right f) = readFile f

checkSource s = do case s of
                      Left err -> putStrLn (show err) >> exitWith (ExitFailure 1)
                      Right m -> return m


getShapesNow :: Float -> [ShapeExpr] -> [Shape]
getShapesNow t = map (evalShapeExpr t) 
