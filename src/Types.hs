{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies,GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Types where

import Control.DeepSeq
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

type Point3 = Vec3
type Point = (Float,Float)
type Grid = [Point]


data Vec3 = Vec3 !Float !Float !Float deriving (Show, Eq, Read)
newtype Ray3 = Ray3 (Vec3,Vec3) deriving (Eq, Read, Show)

derivingUnbox "Vec3"
  [t| Vec3 -> (Float,Float,Float) |]
  [| \ (Vec3 x y z) -> (x,y,z) |]
  [| \ (x,y,z) -> Vec3 x y z |] 

data Color = Color !Float !Float !Float deriving (Eq, Show, Read)

instance NFData Color where
  rnf (Color r g b) = rnf r `seq` rnf g `seq` rnf b

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


derivingUnbox "Color"
  [t| Color -> (Float,Float,Float) |]
  [| \ (Color x y z) -> (x,y,z) |]
  [| \ (x,y,z) -> Color x y z |] 

derivingUnbox "Material"
  [t| Material -> (Color,Color,Float,Float,Float,Color) |]
  [| \ (Material u v w x y z) -> (u,v,w,x,y,z) |]
  [| \ (u,v,w,x,y,z) -> Material u v w x y z |] 

data Vertex = Vertex { vVertex   :: !Int
                     , vTexture  :: !Int
                     , vNormal   :: !Int 
                     } deriving (Eq,Show,Read)

--3 vertex indeces and material index
data TriFace = TriFace !Vertex !Vertex !Vertex !Int deriving (Eq,Read,Show)

derivingUnbox "Vertex"
  [t| Vertex -> (Int,Int,Int) |]
  [| \ (Vertex x y z) -> (x,y,z) |]
  [| \ (x,y,z) -> Vertex x y z |] 

derivingUnbox "TriFace"
  [t| TriFace -> (Vertex,Vertex,Vertex,Int) |]
  [| \ (TriFace w x y z) -> (w,x,y,z) |]
  [| \ (w,x,y,z) -> TriFace w x y z |] 

data Mesh = Mesh { meshVertices :: U.Vector Vec3
                 , meshNormals :: U.Vector Vec3
                 , meshMaterials :: U.Vector Material
                 , meshTriFaces :: U.Vector TriFace
                 } deriving (Eq,Read,Show)

data Object = Sphere { spherePos :: !Vec3
                     , sphereRad :: !Float
                     , sphereMat :: !Material }
            
            | Triangle { triangleA :: !Vec3 --three points
                       , triangleB :: !Vec3
                       , triangleC :: !Vec3
                       , triangleN :: !Vec3 --normal
                       , triangleMat :: !Material }
  deriving (Eq, Show, Read)

data Axis = XAxis | YAxis | ZAxis deriving (Eq,Read,Show)

{- Bounding Box -}
data Box = Box { boxXmin  :: !Float
               , boxXmax  :: !Float
               , boxYmin  :: !Float
               , boxYmax  :: !Float
               , boxZmin  :: !Float
               , boxZmax  :: !Float }
         | EmptyBox deriving (Eq,Show,Read)

{- Bounding Volume Hierarchy -}
data BVH = Node !BVH !BVH !Box 
         | Leaf ![Object] !Box
         | Empty
         deriving (Eq,Show,Read)

data Light = Light !Vec3  --a corner of the light
                   !Vec3  --first edge of light
                   !Vec3  --second edge of light
                   !Color
                   deriving (Eq,Show,Read)


{- Hessian normal form: 
-   dot n x = b where n is normal and b is offset
-   distance = dot n x - b
-}
data Frustum = Frustum { fBotOffset   :: !Float
                       , fTopOffset   :: !Float
                       , fLeftOffset  :: !Float
                       , fRightOffset :: !Float
                       , fBotN        :: !Vec3
                       , fTopN        :: !Vec3
                       , fLeftN       :: !Vec3
                       , fRightN      :: !Vec3
                       } deriving (Eq,Read,Show)

data Packet = Packet !Frustum [Ray3] deriving (Eq,Read,Show)

data World = World { wImgWd :: Float
                   , wImgHt :: Float
                   , wViewWd :: Float
                   , wViewHt :: Float
                   , wViewDt :: Float
                   , wAntiAliasing :: Int --sqrt of number of rays to cast
                   , wDOF :: Int --number of rays for depth of field
                   , wLens :: Float --length of side of lens (0 for pinhole)
                   , wUp :: Vec3
                   , wEye :: Vec3
                   , wCamera :: (Vec3,Vec3,Vec3)
                   , wObjects :: !BVH
                   , wAmbient :: Color
                   , wLights :: [Light]
                   , wMaxDepth :: Int
                   } deriving Show
