{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies,GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Surfaces where

import Control.DeepSeq
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving


data Color = Color !Float !Float !Float deriving (Eq, Show, Read)

instance NFData Color where
  rnf (Color r g b) = rnf r `seq` rnf g `seq` rnf b


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


{- Ambient, Diffuse, Specular, Blinn-Phong, Reflection, Refractive Index, Attenuation, Glossiness -}
data Material = 
  Material 
    { matDiffuse  :: !Color  --diffuse color
    , matSpecular :: !Color  --specular color
    , matPhong    :: !Float  --phong exponent (dull: 10, mildly shiny: 100,
                             --                very glossy 1,000, mirror: 10,000)
    , matRefl     :: !Float  --reflection index [0-1]
    , matRefrIx   :: !Float  --refraction index
    , matAtten    :: !Color  --attenuation
    }
  deriving (Eq, Show, Read)

{- smart constructor that applies e^-ln to the attenuation constants -}
makeMaterial :: Color -> Color -> Float -> Float -> Float -> Color -> Material
makeMaterial diffuse specular phong reflIx refrIx atten =
  Material diffuse specular phong reflIx refrIx (scaleColor log atten)

derivingUnbox "Color"
  [t| Color -> (Float,Float,Float) |]
  [| \ (Color x y z) -> (x,y,z) |]
  [| \ (x,y,z) -> Color x y z |] 

derivingUnbox "Material"
  [t| Material -> (Color,Color,Float,Float,Float,Color) |]
  [| \ (Material u v w x y z) -> (u,v,w,x,y,z) |]
  [| \ (u,v,w,x,y,z) -> Material u v w x y z |] 
