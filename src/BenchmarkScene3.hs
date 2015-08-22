module BenchmarkScene3 where

import Types
import Materials
import Objects

bench3Lights :: [Light]
bench3Lights = [Light (Vec3 25 0 25)
                     (Vec3 1 0 0)
                     (Vec3 0 1 0)
                     (Vec3 0.5 0.5 0.5) ]
bt0 :: Vec3
bt0 = Vec3 (-5) (-20) 20
bt1 :: Vec3
bt1 = Vec3 (-5) 20 20
bt2 :: Vec3
bt2 = Vec3 (-5) (-20) (-5)
bt3 :: Vec3
bt3 = Vec3 (-5) 20 (-5)
bt4 :: Vec3
bt4 = Vec3 20 (-20) (-5)
bt5 :: Vec3
bt5 = Vec3 20 20 (-5)


bench3Objects :: [Object]
bench3Objects =
  [ makeTriangle bt0 bt2 bt1 whiteDull
  , makeTriangle bt2 bt3 bt1 whiteDull
  , makeTriangle bt2 bt4 bt3 whiteDull
  , makeTriangle bt4 bt5 bt3 whiteDull
  ] ++ 
  (concat $
  [ makeParallelPiped (Vec3 x y z) (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) greenGlass
       | x <- [-4,0 .. 4] , y <- [-4,0 .. 4] , z <- [12,14 .. 16] ] ++ 
  [ makeParallelPiped (Vec3 x y z) (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) redM
       | x <- [-4,-0 .. 4] , y <- [12,14 .. 16] , z <- [-4,-0 .. 4] ] ++ 
  [ makeParallelPiped (Vec3 x y z) (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) blueM
       | x <- [12,14 .. 16] , y <- [-4,0 .. 4] , z <- [-4,-0 .. 4] ] ++ 
  [ makeParallelPiped (Vec3 x y z) (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) greenDiamond
       | x <- [-4,-0 .. 4] , y <- [-4,-0 .. 4] , z <- [-4,-0 .. 4] ] ++
  [ makeParallelPiped (Vec3 x y z) (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) goldM
       | x <- [-4,0 .. 4] , y <- [-16,-14 .. -12] , z <- [-4,-0 .. 4] ]
   )
