module Objects where

import Geometry3
import Surfaces

data Object = Sphere { spherePos :: !Vec3
                     , sphereRad :: !Float
                     , sphereMat :: !Material }
            
            | Triangle { triangleA :: !Vec3 --three points
                       , triangleB :: !Vec3
                       , triangleC :: !Vec3
                       , triangleN :: !Vec3 --normal
                       , triangleMat :: !Material }
  deriving (Eq, Show, Read)

printObj :: Object -> String
printObj (Sphere c r _) = "Sphere " ++ show c ++ " " ++ show r
printObj (Triangle a b c _ _) = "Triangle " ++ show a ++ " " ++
                                 show b ++ " " ++ show c


calcNormal :: Vec3 -> Vec3 -> Vec3 -> Vec3
calcNormal a b c = normalize $ cross (subt b a) (subt c a)


{- Smart Constructor for Object that computes triangle normal -}
makeTriangle :: Vec3 -> Vec3 -> Vec3 -> Material -> Object
makeTriangle a b c = Triangle a b c (calcNormal a b c)

makeParallelPiped :: Vec3     -- corner point
                  -> Vec3     -- width vector
                  -> Vec3     -- height vector
                  -> Vec3     -- depth vector
                  -> Material 
                  -> [Object] -- list of triangles that make cuboid
makeParallelPiped p0 wd ht dp m = ts
  where
    p1 = add p0 wd
    p2 = add p0 ht
    p3 = add p1 ht
    p4 = add p0 dp
    p5 = add p1 dp
    p6 = add p2 dp
    p7 = add p3 dp
    t0 = makeTriangle p0 p1 p2 m --front face
    t1 = makeTriangle p1 p3 p2 m
    t2 = makeTriangle p0 p5 p1 m --bottom face
    t3 = makeTriangle p0 p4 p5 m
    t4 = makeTriangle p0 p2 p4 m --left side face
    t5 = makeTriangle p2 p6 p4 m
    t6 = makeTriangle p2 p3 p6 m --top face
    t7 = makeTriangle p3 p7 p6 m
    t8 = makeTriangle p3 p5 p7 m --right face
    t9 = makeTriangle p3 p1 p5 m
    t10 = makeTriangle p4 p6 p5 m --back face
    t11 = makeTriangle p5 p6 p7 m
    ts = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11]
