module BenchmarkScene4 where

{- A scene to highlight depth of field -}


import Geometry3
import Objects
import Materials
import Surfaces
import RayTracer

bench4Lights :: [Light]
bench4Lights = [Light (Vec3 20 20 (-20))
                      (Vec3 0 1 0)
                      (Vec3 0 0 1)
                      (Color 0.8 0.8 0.8) ]

b4t0 :: Vec3
b4t0 = Vec3 50 0 50
b4t1 :: Vec3
b4t1 = Vec3 50 0 (-50)
b4t2 :: Vec3 
b4t2 = Vec3 (-50) 0 50
b4t3 :: Vec3 
b4t3 = Vec3 (-50) 0 (-50)


bench4Objects :: [Object]
bench4Objects =
    [ makeTriangle b4t0 b4t3 b4t2 whiteDull
    , makeTriangle b4t0 b4t1 b4t3 whiteDull
    , Sphere (Vec3 (-40) 7 (-20)) 1 goldM
    , Sphere (Vec3 0 7 (-5)) 1 goldM
    , Sphere (Vec3 40 7 3) 1 goldM
    ] ++
    concat
      [ makeParallelPiped (Vec3 x 0 z) (Vec3 2 0 0) (Vec3 0 5 0) (Vec3 0 0 1)  redDull
        | x <- [-50,-45 .. 50], z <- [-20,-18 .. 20], 5 * z / 2 == x ]
    --  [ Sphere (Vec3 x 2 z) 1  redD
    --    | x <- [-50,-45 .. 50], z <- [-20,-18 .. 20], 5 * z / 2 == x ]
