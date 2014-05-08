module Geometry3 where
  
  {- Vectors are 3-tuples of floats x,y,z -}
  type Vec3 = (Float , Float , Float)
  {- Points are 3-tuples of floats x,y,z -}
  type Pt3 = (Float , Float , Float)
  {- Rays are a tuple of a point and a vector -}
  newtype Ray3 = Ray3 (Pt3 , Vec3) deriving Show
  {- Spheres are tuples of a point and a radius -}

  add :: Vec3 -> Vec3 -> Vec3
  add (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

  subt :: Vec3 -> Vec3 -> Vec3
  subt (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

  multiply :: Vec3 -> Float -> Vec3
  multiply (x, y, z) f = (f*x, f*y, f*z)

  divide :: Vec3 -> Float -> Vec3
  divide (x, y, z) f = (x/f , y/f , z/f)
  
  magnitude :: Vec3 -> Float
  magnitude (x, y, z) = sqrt (x*x + y*y + z*z)
  
  magnitude2 :: Vec3 -> Float
  magnitude2 (x, y, z) = x*x + y*y + z*z

  dot :: Vec3 -> Vec3 -> Float
  dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

  cross :: Vec3 -> Vec3 -> Vec3
  cross (x1, y1 , z1) (x2, y2, z2) = (y1*z2-z1*y2,z1*x2-x1*z2,x1*y2-y1*x2)

  normalize :: Vec3 -> Vec3
  normalize v = divide v (magnitude v)


