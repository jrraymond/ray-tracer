module HaObj where

import Control.Monad
import Data.List (partition,unzip7)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.IO (hGetContents, IOMode (..),withFile)
import Text.Parsec

--TODO switch to arrays/vectors
data V3 = V3 !Float !Float !Float deriving (Eq,Read,Show)

--TODO incomplete, at least need to add texture maps
data Material = Material { mNs    :: !Float   --specular exponent
                         , mKa    :: !V3      --ambient color
                         , mKd    :: !V3      --diffuse color
                         , mKs    :: !V3      --specular color
                         , mNi    :: !Float   --optical density (refraction)
                         , mD     :: !Float    --dissolved (halo factor)
                         , mIllum :: !Int     --illumination model
                         } deriving (Eq,Read,Show)

data Mesh = Mesh { mVertices     :: [V3]
                 , mNormals      :: [V3]
                 , mTextures     :: [V3]
                 , mParamSpaceVs :: [V3]
                 , mFaces        :: [(Material,[Face])]
                 , mSmoothFaces  :: [(Material,[Face])]
                 , mLines        :: [(Int,Int)]
                 } deriving (Eq,Read,Show)

--material index and vertices
data Face = Face [FaceV] deriving (Eq,Read,Show)


data FaceV = FaceV { fvVertex   :: !Int
                   , fvTexture  :: !Int
                   , fvNormal   :: !Int 
                   } deriving (Eq,Show,Read)


makeScene :: [(String, Material)]
          -> [(String,[V3],[V3],[V3],[V3],[(String,Int,[Face])],[(Int,Int)])]
          -> Mesh
makeScene mtls0 objs0 =
  let mtls = M.fromList mtls0
      (_,vs,ns,ts,pss,mfaces,ls) = unzip7 objs0
      mfaces' = map (\(m,s,fs) -> (findMaterial mtls m,s,fs)) (concat mfaces)
      (notSmooth,smooth) = partition (\(_,s,_) -> s == 0) mfaces'
      notSmooth' = map dropMid notSmooth
      smooth' = map dropMid smooth
  in Mesh (concat vs) (concat ns) (concat ts) (concat pss) notSmooth' smooth' (concat ls)

findMaterial :: (Ord a, Show a) => Map a b -> a -> b
findMaterial dict m = fromMaybe (error (show m ++ " not found")) (M.lookup m dict)

dropMid :: (a,b,c) -> (a,c)
dropMid (a,_,c) = (a,c)

{- parses .obj files -}
parseObj fname = (fmap . fmap) (uncurry makeScene) (parseObj' fname)

{- if you don't want to use the types -}
parseObj' fname = withFile fname ReadMode (\h -> do
  input <- hGetContents h
  case runParser readObj () fname input of
    Left e -> return $ Left e
    Right (mtllib,meshes) -> withFile mtllib ReadMode (\h2 -> do
      mtlInput <- hGetContents h2
      case runParser readMtl () mtllib mtlInput of
        Left e -> return $ Left e
        Right mtls -> return $ Right (mtls,meshes)))
  
readObj = do
  skipMany ignorable
  mtllib <- string "mtllib" >> spaces >> manyTill anyChar endOfLine
  meshes <- manyTill (skipMany ignorable >> mesh) eof
  return (mtllib,meshes)

mesh = do
  char 'o'
  spaces
  name <- manyTill anyChar endOfLine
  vs <- many (try vertex)
  ts <- many (try texture)
  ns <- many (try normal)
  pss <- many (try parameterSpace)
  mfaces <- many materialAndFaces
  ls <- many (try line)
  return (name,vs,ns,ts,pss,mfaces,ls)

vertex = char 'v' >> spaces >> parseV3

normal = string "vn" >> spaces >> parseV3

texture = string "vt" >> spaces >> parseV3'

line = do
  string "l"
  v1 <- spaces >> uint
  v2 <- spaces >> uint
  endOfLine
  return (v1,v2)


parameterSpace = string "vp" >> spaces >> parseV3''

materialAndFaces = do
  mtl <- string "usemtl" >> spaces >> manyTill anyChar endOfLine
  s <- try (string "s" >> spaces >> (try (string "off" >> return 0) <|> uint))
       <|> return 0
  faces <- skipMany endOfLine >> many1 ngonFace
  return (mtl, s, faces)

ngonFace = do 
  char 'f'
  spaces
  vs <- sepEndBy faceEl spaces
  return (Face vs)

faceEl = do
  v <- uint
  vt <- try (char '/' >> uint) <|>
        return (-1)
  vn <- try (char '/' >> uint) <|>
        try (char '/' >> char '/' >> uint) <|>
        return (-1)
  return (FaceV v vt vn)

vertexPair = do
  v1 <- uint
  v2 <- char '/' >> uint
  return (v1,v2)

readMtl = do
  skipMany ignorable 
  manyTill (do skipMany ignorable 
               m <- material
               skipMany ignorable
               return m) eof

comments = char '#' >> manyTill anyToken endOfLine >> return ()

ignorable = comments <|> void endOfLine

material = do
  string "newmtl"
  spaces
  name <- manyTill anyChar endOfLine
  ns <- parseNs
  endOfLine
  ka <- parseKa
  kd <- parseKd
  ks <- parseKs
  ni <- try parseNi <* endOfLine <|> return 1.0
  --endOfLine
  d <- parseD
  endOfLine
  illum <- parseIllum
  return (name,Material ns ka kd ks ni d illum)

parseNs = string "Ns" >> spaces >> float

parseKa = string "Ka" >> spaces >> parseV3

parseKd = string "Kd" >> spaces >> parseV3

parseKs = string "Ks" >> spaces >> parseV3

parseNi = string "Ni" >> spaces >> float

parseD = string "d" >> spaces >> float

parseIllum = string "illum" >> spaces >> uint

parseV3 = do
  xs <- sepEndBy float spaces
  case xs of
    (r:g:b:[]) -> return (V3 r g b)
    _ -> fail "unexpected number of floats"

parseV3' = do
  xs <- sepEndBy float spaces
  case xs of
    (u:v:[]) -> return (V3 u v 0)
    (u:v:w:[]) -> return (V3 u v w)
    _ -> fail "unexpected number of floats"

parseV3'' = do
  xs <- sepEndBy float spaces
  case xs of
    (u:[]) -> return (V3 u 0 0)
    (u:v:[]) -> return (V3 u v 0)
    (u:v:w:[]) -> return (V3 u v w)
    _ -> fail "unexpected number of floats"

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

uint = let rd = read :: String -> Int in rd <$> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer
