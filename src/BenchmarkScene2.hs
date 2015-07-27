module BenchmarkScene2 where

import Geometry3
import Materials
import Objects
import Surfaces
import RayTracer

bench2Lights :: [Light]
bench2Lights = [Light (Vec3 10 10 20)
                     (Vec3 5 0 0)
                     (Vec3 0 5 0)
                     (Color 0.5 0.5 0.5) ]


t0a :: Vec3
t0a = Vec3 8 0 (-4)
t0b :: Vec3
t0b = Vec3 0 3 0
t0c :: Vec3
t0c = Vec3 0 0 0
t1a :: Vec3
t1a = Vec3 8 0 (-4)
t1b :: Vec3
t1b = Vec3 8 3 (-4)
t1c :: Vec3
t1c = Vec3 0 3 0
t2a :: Vec3
t2a = Vec3 (-4) 0 8
t2b :: Vec3
t2b = Vec3 0 0 0
t2c :: Vec3
t2c = Vec3 0 3 0
t3a :: Vec3
t3a = Vec3 (-4) 0 8
t3b :: Vec3
t3b = Vec3 0 3 0
t3c :: Vec3
t3c = Vec3 (-4) 3 8

bench2Objects :: [Object]
bench2Objects = makeParallelPiped (Vec3 0 0 14)
                                 (Vec3 4 0 0)
                                 (Vec3 0 4 0)
                                 (Vec3 0 0 (-1))
                                 glass ++
               makeParallelPiped (Vec3 4 0 10)
                                 (Vec3 4 0 0)
                                 (Vec3 0 4 0)
                                 (Vec3 0 0 (-1))
                                 glass ++
               makeParallelPiped (Vec3 8 0 6)
                                 (Vec3 4 0 0)
                                 (Vec3 0 4 0)
                                 (Vec3 0 0 (-1))
                                 glass ++
               makeParallelPiped (Vec3 12 0 2)
                                 (Vec3 4 0 0)
                                 (Vec3 0 4 0)
                                 (Vec3 0 0 (-1))
                                 greenGlass ++
               makeParallelPiped (Vec3 10 0 16)
                                 (Vec3 1 0 0)
                                 (Vec3 0 2 0)
                                 (Vec3 0 0 (-1))
                                 diamond ++
               makeParallelPiped (Vec3 12 0 14)
                                 (Vec3 1 0 0)
                                 (Vec3 0 2 0)
                                 (Vec3 0 0 (-1))
                                 diamond ++
               makeParallelPiped (Vec3 14 0 12)
                                 (Vec3 1 0 0)
                                 (Vec3 0 2 0)
                                 (Vec3 0 0 (-1))
                                 diamond ++
               makeParallelPiped (Vec3 16 0 10)
                                 (Vec3 1 0 0)
                                 (Vec3 0 2 0)
                                 (Vec3 0 0 (-1))
                                 greenDiamond ++
             [ Sphere (Vec3 0 0 0) 2 purpleDull
             , Sphere (Vec3 2 1 10) 1 redM
             , Sphere (Vec3 1 7.5 1) 5 blueM
             , Sphere (Vec3 10 2 4) 1 glass
             , Triangle (Vec3 (-20) 0 (-20))
                        (Vec3 20 0 20)
                        (Vec3 20 0 (-20))
                        (Vec3 0 1 0)
                        whiteDull
             , Triangle (Vec3 (-20) 0 (-20))
                        (Vec3 (-20) 0 20)
                        (Vec3 20 0 20)
                        (Vec3 0 1 0)
                        whiteDull
             , Triangle t0a
                        t0b
                        t0c
                        (calcNormal t0a t0b t0c)
                        goldD
             , Triangle t1a
                        t1b
                        t1c
                        (calcNormal t1a t1b t1c)
                        goldD
             , Triangle t2a
                        t2b
                        t2c
                        (calcNormal t2a t2b t2c)
                        goldD
             , Triangle t3a
                        t3b
                        t3c
                        (calcNormal t3a t3b t3c)
                        goldD
              ]
