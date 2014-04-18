{- @author Brian Gapinske
-  @author Justin Raymond
-
-  Ray Tracer
-}
import qualified Graphics.UI.GLUT as Glut





main :: IO ()
main = do 
          --initialize OpenGL systems
    (_progName, _args) <- Glut.getArgsAndInitialize
    --open the main window
    _window <- Glut.createWindow "Ray Tracer"
    --set the display callback for the main window
    Glut.displayCallback Glut.$= display
    --let glut take over
    Glut.mainLoop

display :: Glut.DisplayCallback
display = do
    --clears out the graphics color state
    Glut.clear [ Glut.ColorBuffer ]
    --pushes our OpenGL commands down to the systems graphics for display
    Glut.flush



