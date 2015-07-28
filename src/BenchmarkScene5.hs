module BenchmarkScene5 where

{- A scene to highlight glossy reflection -}

import Geometry3
import Objects
import Materials
import Surfaces
import RayTracer

bench5Lights :: [Light]
bench5Lights = [Light (Vec3 20 0 0)
                      (Vec3 0 1 0)
                      (Vec3 0 0 1)
                      (Color 0.8 0.8 0.8) ]


bench5Objects :: [Object]
bench5Objects = 
    [ Sphere (Vec3 1 0 0) 3 whiteMirror
    , Sphere (Vec3 5 (-4) 4) 3 redM
    , Sphere (Vec3 5 (-4) (-4)) 3 darkgreyM
    , Sphere (Vec3 5 4 4) 3 greenM
    , Sphere (Vec3 5 4 (-4)) 3 greyM
    ]

