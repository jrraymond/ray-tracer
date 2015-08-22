module Surfaces where

import Types

import qualified Data.Vector.Unboxed as U

mixColors :: (Float -> Float -> Float) -> Color -> Color -> Color
mixColors f (Vec3 r0 g0 b0) (Vec3 r1 g1 b1) = Vec3 (f r0 r1) (f g0 g1) (f b0 b1)
{-# INLINE mixColors #-}

scaleColor :: (Float -> Float) -> Color -> Color
scaleColor f (Vec3 r g b) = Vec3 (f r) (f g) (f b)
{-# INLINE scaleColor #-}

avgColors :: U.Vector Color -> Color
avgColors cs = let mixed = U.foldl' (mixColors (+)) (Vec3 0 0 0) cs
               in scaleColor (/ fromIntegral (U.length cs)) mixed
{-# INLINE avgColors #-}


{- smart constructor that applies e^-ln to the attenuation constants -}
makeMaterial :: Color -> Color -> Float -> Float -> Float -> Color -> Material
makeMaterial diffuse specular phong reflIx refrIx atten =
  Material diffuse specular phong reflIx refrIx (scaleColor log atten)
