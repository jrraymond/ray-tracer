module GlassCubesScene where

import Materials
import Objects
import Geometry3
import Surfaces
import RayTracer

glassLights :: [Light]
glassLights = [Light (Vec3 10 10 20)
                     (Vec3 5 0 0)
                     (Vec3 0 5 0)
                     (Color 0.5 0.5 0.5) ]

glassObjects :: [Object]
glassObjects = makeParallelPiped (Vec3 0 0 15)
                                 (Vec3 5 0 0)
                                 (Vec3 0 5 0)
                                 (Vec3 0 0 (-1))
                                 glass ++
               makeParallelPiped (Vec3 5 0 10)
                                 (Vec3 5 0 0)
                                 (Vec3 0 5 0)
                                 (Vec3 0 0 (-1))
                                 glass ++ 
               makeParallelPiped (Vec3 10 0 5)
                                 (Vec3 5 0 0)
                                 (Vec3 0 5 0)
                                 (Vec3 0 0 (-1))
                                 glass ++ 
               makeParallelPiped (Vec3 15 0 10)
                                 (Vec3 1 0 0)
                                 (Vec3 0 1 0)
                                 (Vec3 0 0 (-1))
                                 greenGlass ++ 
               makeParallelPiped (Vec3 18 0 10)
                                 (Vec3 1 0 0)
                                 (Vec3 0 1 0)
                                 (Vec3 0 0 (-1))
                                 greenDiamond ++ 
               makeParallelPiped (Vec3 13 0 15)
                                 (Vec3 1 0 0)
                                 (Vec3 0 1 0)
                                 (Vec3 0 0 (-1))
                                 glass ++ 
               makeParallelPiped (Vec3 16 0 15)
                                 (Vec3 1 0 0)
                                 (Vec3 0 1 0)
                                 (Vec3 0 0 (-1))
                                 diamond ++
               [ makeTriangle (Vec3 0 0 0)
                              (Vec3 20 0 20)
                              (Vec3 20 0 0)
                              whiteDull
               , makeTriangle (Vec3 0 0 0)
                              (Vec3 0 0 20)
                              (Vec3 20 0 20)
                              whiteDull
               , makeTriangle (Vec3 0 0 0)
                              (Vec3 0 20 20)
                              (Vec3 0 0 20)
                              redDull
               , makeTriangle (Vec3 0 0 0)
                              (Vec3 0 20 0)
                              (Vec3 0 20 20)
                              redDull
               , makeTriangle (Vec3 0 0 0)
                              (Vec3 20 0 0)
                              (Vec3 20 20 0)
                              blueDull
               , makeTriangle (Vec3 0 0 0)
                              (Vec3 20 20 0)
                              (Vec3 0 20 0)
                              blueDull
               , Sphere (Vec3 0 0 0) 2 whiteDull
               ]

s0 :: Object
s0 = Sphere (Vec3 10 0 0) 1 redM
s1 :: Object
s1 = Sphere (Vec3 0 10 0) 1 redM
s2 :: Object
s2 = Sphere (Vec3 0 0 10) 1 redM
s3 :: Object
s3 = Sphere (Vec3 0 (-2) 10) 1 redM
s4 :: Object
s4 = Sphere (Vec3 0 2 10) 1 redM
s5 :: Object
s5 = Sphere (Vec3 0 0 12) 1 redM
