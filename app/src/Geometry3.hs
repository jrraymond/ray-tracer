module Geometry3 where

import Types


add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
{-# INLINE add #-}

subt :: Vec3 -> Vec3 -> Vec3
subt (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
{-# INLINE subt #-}

multiply :: Vec3 -> Float -> Vec3
multiply (Vec3 x y z) f = Vec3 (f * x) (f * y) (f * z)
{-# INLINE multiply #-}

divide :: Vec3 -> Float -> Vec3
divide (Vec3 x y z) f = Vec3 (x / f) (y / f) (z / f)
{-# INLINE divide #-}

magnitude :: Vec3 -> Float
magnitude (Vec3 x y z) = sqrt (x * x + y * y + z * z)
{-# INLINE magnitude #-}

magnitude2 :: Vec3 -> Float
magnitude2 (Vec3 x y z) = x * x + y * y + z * z
{-# INLINE magnitude2 #-}

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
{-# INLINE dot #-}

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)
{-# INLINE cross #-}

normalize :: Vec3 -> Vec3
normalize v = divide v (magnitude v)
{-# INLINE normalize #-}

vecM :: (Float -> Float) -> Vec3 -> Vec3
vecM f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
{-# INLINE vecM #-}

inf :: Float
inf = read "Infinity"

okVec3 :: Vec3 -> Bool
okVec3 (Vec3 x y z) = not (any isNaN [x,y,z])
