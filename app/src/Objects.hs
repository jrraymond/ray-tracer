module Objects where

import Geometry3
import Types


printObj :: Object -> String
printObj (Sphere c r _) = "Sphere " ++ show c ++ " " ++ show r
printObj (Triangle a b c _ _) = "Triangle " ++ show a ++ " " ++
                                 show b ++ " " ++ show c


calcNormal :: Vec3 -> Vec3 -> Vec3 -> Vec3
calcNormal a b c = normalize $ cross (subt b a) (subt c a)


{- Smart Constructor for Object that computes triangle normal -}
makeTriangle :: Vec3 -> Vec3 -> Vec3 -> Material -> Object
makeTriangle a b c = Triangle a b c (calcNormal a b c)

okObject :: Object -> Bool
okObject Sphere{} = True
okObject (Triangle a b c n _) = a /= b && b /= c && a /= c && okVec3 n


makeParallelPiped :: Vec3     -- corner point
                  -> Vec3     -- width vector (must be positive)
                  -> Vec3     -- height vector (must be positive)
                  -> Vec3     -- depth vector (must be positive)
                  -> Material 
                  -> [Object] -- list of triangles that make cuboid
makeParallelPiped p0 wd0 ht0 dp0 m = ts
  where
    (wd,ht,dp) = mapT3 (vecM id) (wd0,ht0,dp0)
    p1 = add p0 wd
    p2 = add p0 ht
    p3 = add p1 ht
    p4 = add p0 dp
    p5 = add p1 dp
    p6 = add p2 dp
    p7 = add p3 dp
    t0 = makeTriangle p0 p2 p1 m --back face
    t1 = makeTriangle p1 p2 p3 m
    t2 = makeTriangle p0 p1 p5 m --bottom face
    t3 = makeTriangle p0 p5 p4 m
    t4 = makeTriangle p0 p4 p2 m --left side face
    t5 = makeTriangle p2 p4 p6 m
    t6 = makeTriangle p2 p6 p3 m --top face
    t7 = makeTriangle p3 p6 p7 m
    t8 = makeTriangle p3 p7 p5 m --right face
    t9 = makeTriangle p3 p5 p1 m
    t10 = makeTriangle p4 p5 p6 m --front face
    t11 = makeTriangle p5 p7 p6 m
    ts = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11]

mapT3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapT3 f (a,b,c) = (f a, f b, f c)
