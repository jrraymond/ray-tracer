module Surfaces where

import Data.List (foldl')

import Types

mixColors :: (Float -> Float -> Float) -> Color -> Color -> Color
mixColors f (Color r0 g0 b0) (Color r1 g1 b1) = Color (f r0 r1) (f g0 g1) (f b0 b1)
{-# INLINE mixColors #-}

scaleColor :: (Float -> Float) -> Color -> Color
scaleColor f (Color r g b) = Color (f r) (f g) (f b)
{-# INLINE scaleColor #-}

avgColors :: [Color] -> Color
avgColors cs = let mixed = foldl' (mixColors (+)) (Color 0 0 0) cs
               in scaleColor (/ fromIntegral (length cs)) mixed
{-# INLINE avgColors #-}


{- smart constructor that applies e^-ln to the attenuation constants -}
makeMaterial :: Color -> Color -> Float -> Float -> Float -> Color -> Material
makeMaterial diffuse specular phong reflIx refrIx atten =
  Material diffuse specular phong reflIx refrIx (scaleColor log atten)
