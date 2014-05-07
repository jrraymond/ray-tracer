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
import Data.List (sortBy)


main :: IO ()
main = do 
    --for debuggin purposes draw a ppm
    --initialize OpenGL systems
    (_progName, _args) <- GLUT.getArgsAndInitialize
    ----open the main window
    _window <- GLUT.createWindow "Ray Tracer"
    GLUT.windowSize GLUT.$= GL.Size 800 600
    ----set the display callback for the main window
    GLUT.displayCallback GLUT.$= display
    GLUT.reshapeCallback GLUT.$= Just reshape
    ----let GLUT take over
    GLUT.mainLoop
reshape :: GLUT.ReshapeCallback
reshape size = GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)

display :: GLUT.DisplayCallback
display = do
    --clears out the graphics color state
    GLUT.clear [ GLUT.ColorBuffer ]
    --(GL.Size x y) <- GLUT.get GLUT.windowSize
    let wd = 800
        ht = 600
        pixels = render wd ht 
    writePPM "output.ppm" wd ht $ invertY wd ht pixels
    print pixels
    arr <- newArray (flatten pixels) :: IO (Ptr Float)
    --arr <- FMU.new (VS.replicate 100 (1 :: Float))
    GL.drawPixels (GL.Size 800 600) (PixelData GL.RGB GL.Float arr)
    --GL.drawPixels size undefined
    --pushes our OpenGL commands down to the systems graphics for display
    GLUT.flush
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
