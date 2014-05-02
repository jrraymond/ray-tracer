{- @author Brian Gapinski
-  @author Justin Raymond
-
-  Ray Tracer
-}
{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import qualified Foreign.Marshal.Utils as FMU
import qualified Data.Vector.Storable as VS
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import qualified Control.Monad.ST as STM
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do 
    --initialize OpenGL systems
    (_progName, _args) <- GLUT.getArgsAndInitialize
    --open the main window
    _window <- GLUT.createWindow "Ray Tracer"
    --set the display callback for the main window
    GLUT.displayCallback GLUT.$= display
    GLUT.reshapeCallback GLUT.$= Just reshape
    --let GLUT take over
    GLUT.mainLoop

reshape :: GLUT.ReshapeCallback
reshape size = do GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)

display :: GLUT.DisplayCallback
display = do
    --clears out the graphics color state
    GLUT.clear [ GLUT.ColorBuffer ]
    (GL.Size x y) <- GLUT.get GLUT.windowSize
    arr <- newArray (replicate 2500 0.555) :: IO (Ptr Float)
    --arr <- FMU.new (VS.replicate 100 (1 :: Float))
    GL.drawPixels (GL.Size 50 50) (PixelData GL.RGBA GL.Float arr)
    --GL.drawPixels size undefined
    --pushes our OpenGL commands down to the systems graphics for display
    GLUT.flush
