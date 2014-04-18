{- @author Brian Gapinske
-  @author Justin Raymond
-
-  Ray Tracer
-}
import qualified Graphics.UI.GLUT as Glut
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do 
    --initialize OpenGL systems
    (_progName, _args) <- Glut.getArgsAndInitialize
    --open the main window
    _window <- Glut.createWindow "Ray Tracer"
    --set the display callback for the main window
    Glut.displayCallback Glut.$= display
    Glut.reshapeCallback Glut.$= Just reshape
    --let glut take over
    Glut.mainLoop

reshape :: Glut.ReshapeCallback
reshape size = do Glut.viewport Glut.$= (Glut.Position 0 0, size)
display :: Glut.DisplayCallback
display = do
    --clears out the graphics color state
    Glut.clear [ Glut.ColorBuffer ]
    Glut.renderPrimitive Glut.Points $
         mapM_ (\(x, y, z) -> Glut.vertex $ Glut.Vertex3 x y z) myPoints
    --pushes our OpenGL commands down to the systems graphics for display
    Glut.flush


mCF :: Glut.GLfloat
mCF = snd $ maxVal $ firstNcollatz 1000
mCS = fromIntegral $ fst $ maxVal $ firstNcollatz 1000

myPoints :: [(Glut.GLfloat, Glut.GLfloat, Glut.GLfloat)]
myPoints = zipWith (\(x,y) z -> (((fromIntegral x) / (0.5*mCS)) - 1, ((fromIntegral y) / (0.5*mCF)) - 1,z)) (Map.toList (firstNcollatz 1000)) (cycle [0])


--store computed collatz numbers in a Map
collatz :: (Integral a1, Num a) => a1 -> a -> Map a1 a -> a
collatz 1 i t = i
collatz n i t
  | n `Map.member` t = ((Map.!) t n) + i - 1
  | even n = collatz (n `div` 2) (i+1) t
  | otherwise = collatz (3*n+1) (i+1) t

addCollatz :: (Integral a1, Num a) => a1 -> Map a1 a -> Map a1 a
addCollatz n t = Map.insert n (collatz n 1 t) t

maxVal :: (Integral a1, Num a, Ord a) => Map a1 a -> (a1, a)
maxVal t = max' l (head l) where
  l = Map.toList t
  max' [] p = p
  max' ((k',v'):xs) (k,v) 
    | v >= v'   = max' xs (k,v) 
    | otherwise = max' xs (k',v')

firstNcollatz n = foldr addCollatz Map.empty [1..n]
