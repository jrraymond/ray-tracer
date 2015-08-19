module Main where

import Geometry3
import Objects
import Materials
import RayTracer

import Criterion.Main

triangles :: [Object]
triangles = [ makeTriangle (Vec3 0 1 0) (Vec3 1 0 0) (Vec3 0 0 1) redM
            , makeTriangle (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1) redM
            , makeTriangle (Vec3 4 0 0) (Vec3 4 0 1) (Vec3 4 1 0) redM ]


rB :: Vec3
rB = Vec3 10 10 10
rD :: Vec3
rD = normalize $ Vec3 (-1) (-1) (-1)

main :: IO ()
main = defaultMain
  [ bgroup "Triangle hit " (zipWith (\s t -> bench (show s) $ nf (hit rB rD) t) [0 :: Int ..] triangles)
  
  , bgroup "Triangle hit2" (zipWith (\s t -> bench (show s) $ nf (hit rB rD) t) [0 :: Int ..] triangles)
  ]
