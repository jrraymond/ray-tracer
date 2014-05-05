{- @author Brian Gapinski
-  @author Justin Raymond
-
-  Ray Tracer
-}
{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import Surfaces
import RayTracer


main :: IO ()
main = do 
    --for debuggin purposes draw a ppm
    writePPM "output.ppm" 800 600 (render 800 600)
    --initialize OpenGL systems
    (_progName, _args) <- GLUT.getArgsAndInitialize
    ----open the main window
    _window <- GLUT.createWindow "Ray Tracer"
    GLUT.windowSize GLUT.$= (GL.Size 800 600)
    ----set the display callback for the main window
    GLUT.displayCallback GLUT.$= display
    GLUT.reshapeCallback GLUT.$= Just reshape
    ----let GLUT take over
    GLUT.mainLoop
reshape :: GLUT.ReshapeCallback
reshape size = do GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)

display :: GLUT.DisplayCallback
display = do
    --clears out the graphics color state
    GLUT.clear [ GLUT.ColorBuffer ]
    --(GL.Size x y) <- GLUT.get GLUT.windowSize
    let pixels = flatten $ render 800 600
    arr <- newArray pixels :: IO (Ptr Float)
    --arr <- FMU.new (VS.replicate 100 (1 :: Float))
    GL.drawPixels (GL.Size 800 600) (PixelData GL.RGB GL.Float arr)
    --GL.drawPixels size undefined
    --pushes our OpenGL commands down to the systems graphics for display
    GLUT.flush
writePPM :: String -> Int -> Int -> [Color] -> IO ()
writePPM name w h pixels = do writeFile name string  where 
  toStr :: Float -> String
  toStr = (++ " ") . show . truncate . (255*)
  f :: [Color] -> String
  f [] = ""
  f ((Color r g b):ps) = toStr r ++ toStr g ++ toStr b ++ f ps
  string = "P3\n" ++ show w ++ " " ++ show h ++ " 255\n" ++ f pixels
{-
save_ppm :: FilePath -> [[Color]] -> IO ()
save_ppm f css = writeFile f $ make_ppm css
 
make_ppm :: [[Color]] -> String
make_ppm css =
      "P3\n" ++ (show $ length $ head css) ++ " " ++ (show $ length css) ++ " 255\n" ++
        (unlines $ map unwords $ group' 15 $ map show $ concatMap colour $ concat css)
         
group' _ [] = []
group' n xs =
      let (xs0,xs1) = splitAt n xs
            in  xs0 : group' n xs1
colour :: Color -> [Int]             
colour (Color (r,g,b)) = [channel r, channel g, channel b]
 
channel :: Float -> Int
channel = floor . (255*) . min 1 . max 0
-}
