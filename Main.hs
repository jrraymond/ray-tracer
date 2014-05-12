{- @author Brian Gapinski
-  @author Justin Raymond
-
-  Ray Tracer
-}
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
import Data.Map (Map)
import qualified Data.Map as Map


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

    colorFin <- checkFile $ optColorF opts
    colorMap <- checkSource $ readColors Map.empty colorFin 
    print colorMap
    matFin <- checkFile $ optMaterialF opts
    matMap <-  checkSource $ readMaterials colorMap matFin
    print matMap
    shapeFin <- checkFile $ optShapeF opts
    shapeMap <- checkSource $ readShapes matMap shapeFin
    print shapeMap

    let (planes',shapes') = partition isPlane $ map snd (Map.toList shapeMap)
        world = World (iwd,iht) (8,6,4) (u,v,w) eye' lookAt' shapes' planes' (makeBbt shapes' AxisX) lts amb as ss rd
        pixels = render world
        display :: GLUT.DisplayCallback
        display = do
          --clears out the graphics color state
          GLUT.clear [ GLUT.ColorBuffer ]
          --(GL.Size x y) <- GLUT.get GLUT.windowSize
          arr <- newArray (flatten pixels) :: IO (Ptr Float)
          --arr <- FMU.new (VS.replicate 100 (1 :: Float))
          GL.drawPixels (GL.Size ciwd ciht) (PixelData GL.RGB GL.Float arr)
          --GL.drawPixels size undefined
          --pushes our OpenGL commands down to the systems graphics for display
          GLUT.flush
    writePPM "output.ppm" iwd iht $ invertY iwd iht pixels
    --GLUT.initialWindowSize GLUT.$= GL.Size ciwd ciht
    ----open the main window
    --_window <- GLUT.createWindow "Ray Tracer"
    ----set the display callback for the main window
    --GLUT.displayCallback GLUT.$= display
    --GLUT.reshapeCallback GLUT.$= Just reshape
    ----let GLUT take over
    --GLUT.mainLoop


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
  { optColorF :: Either String String
  , optMaterialF :: Either String String
  , optShapeF :: Either String String
  , optImgWd :: Maybe Int
  , optImgHt :: Maybe Int
  , optAntiAliasing :: Maybe Int
  , optSoftShadows :: Maybe Int
  , optReflDepth :: Maybe Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optColorF = Left "No color file"
  , optMaterialF = Left "No material file"
  , optShapeF = Left "No shape file"
  , optImgWd = Just 800
  , optImgHt = Just 600
  , optAntiAliasing = Just 1
  , optSoftShadows = Just 0
  , optReflDepth = Just 2
  }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['c'] ["colors"] (ReqArg (\x opts -> opts {optColorF = Right x}) "") "Color file"
    , Option ['m'] ["materials"] (ReqArg (\x opts -> opts { optMaterialF = Right x}) "") "Material file"
    , Option ['s'] ["shapes"] (ReqArg (\x opts -> opts { optShapeF = Right x}) "") "Shape file"
    , Option ['w'] ["imageWidth"] (ReqArg (\w opts -> opts { optImgWd = readInt w}) "800") "image width in pixels"
    , Option ['h'] ["imageHeight"] (ReqArg (\h opts -> opts { optImgHt = readInt h}) "600") "image height in pixels"
    , Option [] ["antialiasing"] (ReqArg (\a opts -> opts { optAntiAliasing = readInt a}) "1") "antialiasing level"
    , Option [] ["softshadows"] (ReqArg (\s opts -> opts { optSoftShadows = readInt s }) "0") "shoftshadow level"
    , Option [] ["reflectiondepth"] (ReqArg (\d opts -> opts { optReflDepth = readInt d}) "2") "reflection depth"
    ]

readInt :: String -> Maybe Int
readInt = fmap fst . listToMaybe . reads

parseOpts :: [String] -> (Options, [String])
parseOpts args =
    case getOpt Permute options args of
      (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
      (_,_,ers) -> (defaultOptions, [concat ers ++ usageInfo header options])
      where header = "Usage: ./raytracer -s[--scene] <scene-file> [-w,-h,--antialiasing,--softshadows]"

checkFile :: Either String String -> IO String
checkFile (Left e) = putStrLn e >> exitWith (ExitFailure 1)
checkFile (Right f) = readFile f

checkSource s = do case s of
                      Left err -> putStrLn (show err) >> exitWith (ExitFailure 1)
                      Right m -> return m








--eye' = (-4, 4, 7)
--lookAt' = (8,4,1)
--up = (0,0,1)
eye' = (25, 2, 25)
lookAt' = (-1,-1,-1)
up = (0,1,0)
w = normalize $ subt eye' lookAt'
u = normalize $ cross up w
v = cross w u
lts = [ ((50, 20, 0), Color 0.5 0.5 0.5)
    , ((3, 2, 20), Color 0.2 0.2 0.2)
    ]
amb = Color 0.1 0.1 0.1
          
