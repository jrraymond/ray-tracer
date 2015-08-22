{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Types where

import Control.DeepSeq
import Control.Monad (liftM,zipWithM_)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Unboxed as U

type Point3 = Vec3
type Grid = [Point]


data Vec3 = Vec3 !Float !Float !Float deriving (Show, Eq, Read)
newtype Ray3 = Ray3 (Vec3,Vec3) deriving (Eq, Read, Show)
type Color = Vec3

instance NFData Vec3 where
  rnf (Vec3 r g b) = rnf r `seq` rnf g `seq` rnf b

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


data Vertex = Vertex { vVertex   :: !Int
                     , vTexture  :: !Int
                     , vNormal   :: !Int 
                     } deriving (Eq,Show,Read)

--3 vertex indeces and material index
data TriFace = TriFace !Vertex !Vertex !Vertex !Int deriving (Eq,Read,Show)

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
               , boxXmax  :: !Float , boxYmin  :: !Float
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

data F6 = F6 {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
             deriving (Eq,Read,Show)

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
                   , wLens :: Float --length of side of lens (0 for pinhole)
                   , wUp :: Vec3
                   , wEye :: Vec3
                   , wCamera :: (Vec3,Vec3,Vec3)
                   , wObjects :: !BVH
                   , wAmbient :: Color
                   , wLights :: [Light]
                   , wMaxDepth :: Int
                   } deriving Show

newtype Point = Point (Float,Float) deriving (Show,Eq,Read)
newtype F4 = F4 (Float,Float,Float,Float) deriving (Show,Eq,Read)

{- for vectors of two floats -}
newtype instance MVector s Point = MV_Point (MVector s Float)
newtype instance Vector Point = V_Point (Vector Float)
instance Unbox Point

instance M.MVector MVector Point where 
  basicLength (MV_Point v) = M.basicLength v `div` 2 
  basicUnsafeSlice a b (MV_Point v) = MV_Point $ M.basicUnsafeSlice (a*2) (b*2) v 
  basicOverlaps (MV_Point v0) (MV_Point v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_Point (M.basicUnsafeNew (2*n))
  basicUnsafeRead (MV_Point v) n = do 
    [a,b] <- mapM (M.basicUnsafeRead v) [2*n,2*n+1]
    return $ Point (a,b)
  basicUnsafeWrite (MV_Point v) n (Point (a,b)) =
    zipWithM_ (M.basicUnsafeWrite v) [2*n,2*n+1] [a,b]

instance G.Vector Vector Point where 
  basicUnsafeFreeze (MV_Point v) = liftM V_Point (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Point v) = liftM MV_Point (G.basicUnsafeThaw v)
  basicLength (V_Point v) = G.basicLength v `div` 2
  basicUnsafeSlice a b (V_Point v) = V_Point $ G.basicUnsafeSlice (a*2) (b*2) v
  basicUnsafeIndexM (V_Point v) n = do 
    [a,b] <- mapM (G.basicUnsafeIndexM v) [2*n,2*n+1]
    return $ Point (a,b) 

{- for vectors of three floats -}
newtype instance MVector s Vec3 = MV_Vec3 (MVector s Float)
newtype instance Vector Vec3 = V_Vec3 (Vector Float)
instance Unbox Vec3

instance M.MVector MVector Vec3 where 
  basicLength (MV_Vec3 v) = M.basicLength v `div` 3 
  basicUnsafeSlice a b (MV_Vec3 v) = MV_Vec3 $ M.basicUnsafeSlice (a*3) (b*3) v 
  basicOverlaps (MV_Vec3 v0) (MV_Vec3 v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_Vec3 (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_Vec3 v) n = do 
    [a,b,c] <- mapM (M.basicUnsafeRead v) [3*n,3*n+1,3*n+2]
    return $ Vec3 a b c
  basicUnsafeWrite (MV_Vec3 v) n (Vec3 a b c) =
    zipWithM_ (M.basicUnsafeWrite v) [3*n,3*n+1,3*n+2] [a,b,c]

instance G.Vector Vector Vec3 where 
  basicUnsafeFreeze (MV_Vec3 v) = liftM V_Vec3 (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Vec3 v) = liftM MV_Vec3 (G.basicUnsafeThaw v)
  basicLength (V_Vec3 v) = G.basicLength v `div` 3
  basicUnsafeSlice a b (V_Vec3 v) = V_Vec3 $ G.basicUnsafeSlice (a*3) (b*3) v
  basicUnsafeIndexM (V_Vec3 v) n = do 
    [a,b,c] <- mapM (G.basicUnsafeIndexM v) [3*n,3*n+1,3*n+2]
    return $ Vec3 a b c 

{- for vectors of rays -}
newtype instance MVector s Ray3 = MV_Ray3 (MVector s Float)
newtype instance Vector Ray3 = V_Ray3 (Vector Float)
instance Unbox Ray3

instance M.MVector MVector Ray3 where 
  basicLength (MV_Ray3 v) = M.basicLength v `div` 6 
  basicUnsafeSlice a b (MV_Ray3 v) = MV_Ray3 $ M.basicUnsafeSlice (a*6) (b*6) v 
  basicOverlaps (MV_Ray3 v0) (MV_Ray3 v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_Ray3 (M.basicUnsafeNew (6*n))
  basicUnsafeRead (MV_Ray3 v) n = do 
    [bu,bv,bw,du,dv,dw] <- mapM (M.basicUnsafeRead v) [6*n+i | i <- [0..5]]
    return $ Ray3 (Vec3 bu bv bw,Vec3 du dv dw)
  basicUnsafeWrite (MV_Ray3 v) n (Ray3 (Vec3 bu bv bw,Vec3 du dv dw)) =
    zipWithM_ (M.basicUnsafeWrite v) [6*n+i | i <- [0..5]] [bu,bv,bw,du,dv,dw]
instance G.Vector Vector Ray3 where 
  basicUnsafeFreeze (MV_Ray3 v) = liftM V_Ray3 (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Ray3 v) = liftM MV_Ray3 (G.basicUnsafeThaw v)
  basicLength (V_Ray3 v) = G.basicLength v `div` 6
  basicUnsafeSlice a b (V_Ray3 v) =
    V_Ray3 $ G.basicUnsafeSlice (a*6) (b*6) v
  basicUnsafeIndexM (V_Ray3 v) n = do 
    [bu,bv,bw,du,dv,dw] <- mapM (G.basicUnsafeIndexM v) [6*n+i | i <- [0..5]]
    return $ Ray3 (Vec3 bu bv bw,Vec3 du dv dw)

{- for vectors of four floats -}
newtype instance MVector s F4 = MV_F4 (MVector s Float)
newtype instance Vector F4 = V_F4 (Vector Float)
instance Unbox F4

instance M.MVector MVector F4 where 
  basicLength (MV_F4 v) = M.basicLength v `div` 4 
  basicUnsafeSlice a b (MV_F4 v) = MV_F4 $ M.basicUnsafeSlice (a*4) (b*4) v 
  basicOverlaps (MV_F4 v0) (MV_F4 v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_F4 (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_F4 v) n = do 
    [a,b,c,d] <- mapM (M.basicUnsafeRead v) [4*n,4*n+1,4*n+2,4*n+3]
    return $ F4 (a,b,c,d)
  basicUnsafeWrite (MV_F4 v) n (F4 (a,b,c,d)) =
    zipWithM_ (M.basicUnsafeWrite v) [4*n,4*n+1,4*n+2,4*n+3] [a,b,c,d]

instance G.Vector Vector F4 where 
  basicUnsafeFreeze (MV_F4 v) = liftM V_F4 (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_F4 v) = liftM MV_F4 (G.basicUnsafeThaw v)
  basicLength (V_F4 v) = G.basicLength v `div` 4
  basicUnsafeSlice a b (V_F4 v) = V_F4 $ G.basicUnsafeSlice (a*4) (b*4) v
  basicUnsafeIndexM (V_F4 v) n = do 
    [a,b,c,d] <- mapM (G.basicUnsafeIndexM v) [4*n,4*n+1,4*n+2,4*n+3]
    return $ F4 (a,b,c,d) 

{- for vectors of three ints -}
newtype instance MVector s Vertex = MV_Vertex (MVector s Int)
newtype instance Vector Vertex = V_Vertex (Vector Int)
instance Unbox Vertex

instance M.MVector MVector Vertex where 
  basicLength (MV_Vertex v) = M.basicLength v `div` 3 
  basicUnsafeSlice a b (MV_Vertex v) = MV_Vertex $ M.basicUnsafeSlice (a*3) (b*3) v 
  basicOverlaps (MV_Vertex v0) (MV_Vertex v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_Vertex (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_Vertex v) n = do 
    [a,b,c] <- mapM (M.basicUnsafeRead v) [3*n,3*n+1,3*n+2]
    return $ Vertex a b c
  basicUnsafeWrite (MV_Vertex v) n (Vertex a b c) =
    zipWithM_ (M.basicUnsafeWrite v) [3*n,3*n+1,3*n+2] [a,b,c]

instance G.Vector Vector Vertex where 
  basicUnsafeFreeze (MV_Vertex v) = liftM V_Vertex (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Vertex v) = liftM MV_Vertex (G.basicUnsafeThaw v)
  basicLength (V_Vertex v) = G.basicLength v `div` 3
  basicUnsafeSlice a b (V_Vertex v) = V_Vertex $ G.basicUnsafeSlice (a*3) (b*3) v 
  basicUnsafeIndexM (V_Vertex v) n = do 
    [a,b,c] <- mapM (G.basicUnsafeIndexM v) [3*n,3*n+1,3*n+2]
    return $ Vertex a b c

{- triface -}
newtype instance MVector s TriFace = MV_TriFace (MVector s Int)
newtype instance Vector TriFace = V_TriFace (Vector Int)
instance Unbox TriFace

instance M.MVector MVector TriFace where 
  basicLength (MV_TriFace v) = M.basicLength v `div` 10 
  basicUnsafeSlice a b (MV_TriFace v) = MV_TriFace $ M.basicUnsafeSlice (a*10) (b*10) v 
  basicOverlaps (MV_TriFace v0) (MV_TriFace v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_TriFace (M.basicUnsafeNew (10*n))
  basicUnsafeRead (MV_TriFace v) n = do 
    [a0,a1,a2,b0,b1,b2,c0,c1,c2,d] <- mapM (M.basicUnsafeRead v) [10*n+i | i <- [0..9]]
    return $ TriFace (Vertex a0 a1 a2) (Vertex b0 b1 b2) (Vertex c0 c1 c2) d
  basicUnsafeWrite (MV_TriFace v) n (TriFace (Vertex a0 a1 a2) (Vertex b0 b1 b2) (Vertex c0 c1 c2) d) =
    zipWithM_ (M.basicUnsafeWrite v) [10*n+i | i <- [0..9]] [a0,a1,a2,b0,b1,b2,c0,c1,c2,d]

instance G.Vector Vector TriFace where 
  basicUnsafeFreeze (MV_TriFace v) = liftM V_TriFace (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_TriFace v) = liftM MV_TriFace (G.basicUnsafeThaw v)
  basicLength (V_TriFace v) = G.basicLength v `div` 10
  basicUnsafeSlice a b (V_TriFace v) = V_TriFace $ G.basicUnsafeSlice (a*10) (b*10) v 
  basicUnsafeIndexM (V_TriFace v) n = do 
    [a0,a1,a2,b0,b1,b2,c0,c1,c2,d] <- mapM (G.basicUnsafeIndexM v) [10*n+i | i <- [0..9]]
    return $ TriFace (Vertex a0 a1 a2) (Vertex b0 b1 b2) (Vertex c0 c1 c2) d

{- for material -}
newtype instance MVector s Material = MV_Material (MVector s Float)
newtype instance Vector Material = V_Material (Vector Float)
instance Unbox Material

instance M.MVector MVector Material where 
  basicLength (MV_Material v) = M.basicLength v `div` 12 
  basicUnsafeSlice a b (MV_Material v) =
    MV_Material $ M.basicUnsafeSlice (a*12) (b*12) v 
  basicOverlaps (MV_Material v0) (MV_Material v1) = M.basicOverlaps v0 v1 
  basicUnsafeNew n = liftM MV_Material (M.basicUnsafeNew (12*n))
  basicUnsafeRead (MV_Material v) n = do 
    [dr,dg,db,sr,sg,sb,ph,rfl,rfr,ar,ag,ab] <- mapM (M.basicUnsafeRead v) [12*n+i | i <- [0..11]]
    return $ Material (Vec3 dr dg db) (Vec3 sr sg sb) ph rfl rfr (Vec3 ar ag ab)
  basicUnsafeWrite (MV_Material v) n (Material (Vec3 dr dg db) (Vec3 sr sg sb) ph rfl rfr (Vec3 ar ag ab)) =
    zipWithM_ (M.basicUnsafeWrite v) [12*n+i | i <- [0..11]] [dr,dg,db,sr,sg,sb,ph,rfl,rfr,ar,ag,ab]

instance G.Vector Vector Material where 
  basicUnsafeFreeze (MV_Material v) = liftM V_Material (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Material v) = liftM MV_Material (G.basicUnsafeThaw v)
  basicLength (V_Material v) = G.basicLength v `div` 12
  basicUnsafeSlice a b (V_Material v) = V_Material $ G.basicUnsafeSlice (a*12) (b*12) v 
  basicUnsafeIndexM (V_Material v) n = do 
    [dr,dg,db,sr,sg,sb,ph,rfl,rfr,ar,ag,ab] <- mapM (G.basicUnsafeIndexM v) [12*n+i | i <- [0..11]]
    return $ Material (Vec3 dr dg db) (Vec3 sr sg sb) ph rfl rfr (Vec3 ar ag ab)
