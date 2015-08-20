module SceneParser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Identity
import Text.Parsec hiding (runParser)
import Text.ParserCombinators.Parsec hiding (try)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
  
{- A record type the scene will be parsed Into -}
data SceneConfig = SceneConfig { cViewPlane :: (Int, Int, Float)
                               , cEye       :: Pt3Expr
                               , cLookAt    :: Pt3Expr
                               , cUp        :: Pt3Expr
                               , cObjects  :: [ObjectExpr]
                               , cPlanes    :: [ObjectExpr]
                               , cLights    :: [LightExpr]
                               , cAmbient   :: Color
                               } deriving Show

{- New types with ASTs for arithmetic expressions with time free in them
- -}
type Vec3Expr = (Expr,Expr,Expr)
type Pt3Expr = Vec3Expr
type LightExpr = (Pt3Expr,Color)

{- A datatype corresponding to our shape datatype with Expressions
- instead of floats. -}
data ObjectExpr = 
    SphereE (Expr,Expr,Expr) Expr Material 
  | PlaneE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
  | TriangleE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
   deriving Show

{- A convenient function. Attempts to parse with p. If p fails backtracks
- and attempts to parse with q -}
(<||>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p <||> q = try p <|> q

{- ------------ Parsing the scene file -------------------
This parses files of the form:
  CONFIG
    EYE {15  2 15}
    LOOKAT { -1 -1 -1}
    UP {0 1 0}
    VIEWPLANE {<width> <height> <dist>} 
    AMBIENT Color 0.1 0.1 0.1
  END
  LIGHTS
    Light {1 1 1} <color>
  END
  COLORS
    Color <name> = 0.2 0.5 0.8
  END
  MATERIALS
    Material <name> = <material info>
  END
  OBJECTS
    <shape> <name> = <information>
  END

  parseScene parses the string into a tuple of the configuration
  variables, Lights, and surfaces
-}
parseScene :: String -> Either String SceneConfig
parseScene s = let configs = checkSceneConfigVars $ parse (skipper configOpt "CONFIG" "END") "" s
                   colormap = parseMap (skipper color "COLORS" "END") () s 
                   lights = colormap >>= (\cmap -> runParser (skipper light "LIGHTS" "END") cmap "" s)
                   objs = colormap >>= (\cmap -> 
                          parseMap (skipper material "MATERIAL" "END") cmap s  >>= (\matmap ->
                          parseMap (skipper shapeExpr "OBJECTS" "END") matmap s ))  
               in if isLeft lights || isLeft objs || isLeft configs
                    then Left $ "Failed parsing: " ++ show configs
                              ++ " " ++ show lights ++ " " ++ show objs 
                    else Right $ buildSceneConfig (right configs) (right lights) (right objs)

{- This constructs the config record type from the parsed string -}
buildSceneConfig :: (Pt3Expr, Pt3Expr, Vec3Expr, (Float,Float,Float), Color) 
                    -> [LightExpr] 
                    -> Map String ObjectExpr 
                    -> SceneConfig
buildSceneConfig (eye,lookat,up,(vpw,vph,vpd),amb) lights shapes = 
  let (planes,objs) = List.partition isPlaneExpr $ Map.elems shapes
  in  SceneConfig (floor vpw, floor vph, vpd) eye lookat up objs planes lights amb




configOpt :: ParsecT String () Identity (String, String)
configOpt = do skipMany space
               k <- optId
               skipMany space
               v <- manyTill anyChar eol
               skipMany space
               return (strip k,strip v)

skipper :: Stream s m Char => ParsecT s u m a -> String -> String -> ParsecT s u m [a]
skipper p start end = manyTill anyChar (try (string start)) >> 
                      manyTill p (try $ string end)

optId :: Stream s m Char => ParsecT s u m String
optId = string "EYE" <|> string "LOOKAT" <||> string "UP"
    <|> string "VIEWPLANE" <|> string "AMBIENT"

eol :: Parser ()
eol = void $ oneOf "\n\r"

parseMap :: Ord k => GenParser tok st [(k, a)] -> st -> [tok] -> Either ParseError (Map k a)
parseMap p m s = case runParser p m "" s of
                        Left err -> Left err
                        Right xs -> Right (Map.fromList xs)

{- A parser to read an identifier for a color, material, or shape -}
ident :: Stream s m Char => ParsecT s u m String
ident = do skipMany space
           c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           skipMany space
           return (c:cs)
           <?> "identifier"

{- Helper parsers to parse numbers -}
number :: Stream s m Char => ParsecT s u m String
number = many1 digit

plus :: Stream s m Char => ParsecT s u m String
plus = char '+' *> number

minus :: Stream s m Char => ParsecT s u m String
minus = (:) <$> char '-' <*> number

integer :: Stream s m Char => ParsecT s u m String
integer = plus <|> minus <|> number

float :: Stream s m Char => ParsecT s u m Float
float = fmap rd $ (++) <$> integer <*> dec where
  rd = read :: String -> Float
  dec = option "" $ (:) <$> char '.' <*> number
{- we need a function that parses a float in reverse for our expression
- language -}
revFloat  :: Stream s m Char => ParsecT s u m Float
revFloat = do ds <- many (noneOf " {}+-/*()t")
              case parse float "" (reverse ds) of
                  Left err -> parserFail $ show err ++ " @revFloat"
                  Right e' -> return e'

{- Parser to read identifier and check if is in the identifier map -}
identify :: Stream s m Char => ParsecT s (Map String a) m a
identify = do skipMany space
              c <- letter <|> char '_'
              cs <- many (letter <|> digit <|> char '_')
              skipMany space
              m <- getState
              return $  fromMaybe (error "unknown id") ((c:cs) `Map.lookup` m)

{- A parser for a light -}
light :: GenParser Char (Map String Color) ((Expr,Expr,Expr),Color)
light = do skipMany space
           _ <- string "Light" 
           skipMany space
           p <- pointExpr 
           c <- color' <||> identify
           return (p,c)

{- A color may either be a color bound to an identifier or an anonymous
- color -}
color :: Stream s m Char => ParsecT s u m (String, Color)
color = do skipMany space
           _ <- string "Color"
           key <- ident
           _ <- char '='
           skipMany space 
           (x,y,z) <- sep3by float (skipMany space)
           skipMany space
           return (key,Color x y z)
{- Anonymous color -}
color' :: Stream s m Char => ParsecT s u m Color
color' = do _ <- string "Color"
            skipMany space
            (x,y,z) <- sep3by float (skipMany space)
            skipMany space
            return (Color x y z)

{- To read all the colors from a stream we parse many colors and collect
- the results into a map. There is no reason to collect anonymous colors
- here -}
readColors :: Stream s Identity Char => t -> s -> Either ParseError (Map String Color)
readColors _ s = case parse (many color) "" s of 
                       Left err -> Left err
                       Right xs -> Right (Map.fromList xs)

{- Parser for materials. Collects results into a Map from identifiers to
- materials. Colors in a material may be anonymous colors or a color
- identifier -}
material :: GenParser Char (Map String Color) (String,Material)
material = do skipMany space 
              _ <- string "Material" 
              key <- ident
              _ <- char '='
              amb <- color' <||> identify
              dif <- color' <||> identify
              spe <- color' <||> identify
              bp <- float
              refl <- color' <||> identify
              refr <- float
              attn <- color' <||> identify
              gloss <- float
              skipMany space
              return (key,(amb, dif, spe, bp, refl, refr, attn, gloss))

{- Parses all materials from a stream. Requeries a Map of identifiers to
- colors -}
readMaterials :: Map String Color -> String -> Either ParseError (Map String Material)
readMaterials m s = case runParser (many material) m "" s of
                      Left err -> Left err
                      Right xs -> Right (Map.fromList xs)

{- A helper function to parse three occurences of something seperated by
- something else. -}
--TODO generalize this replicateM or something
sep3by :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a,a,a)
sep3by p sep = do x <- p
                  _ <- sep
                  y <- p
                  _ <- sep
                  z <- p
                  return (x,y,z)

{- Parses a point into a tuple of expressions. Whitespace deliminated
- seperation.
-  Ex: {x y z}
-}
pointExpr :: Stream s m Char => ParsecT s u m (Expr, Expr, Expr)
pointExpr = do skipMany space
               _ <- char '{'
               skipMany space
               xyz <- sep3by expression (skipMany space)
               _ <- char '}'
               skipMany space
               return xyz
point :: Stream s m Char => ParsecT s u m (Float, Float, Float)
point = do skipMany space
           _ <- char '{'
           skipMany space
           xyz <- sep3by float (skipMany space)
           _ <- char '}'
           skipMany space
           return xyz

sphereExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ObjectExpr)
sphereExpr = do _ <- string "Sphere"
                key <- ident
                _ <- char '='
                center <- pointExpr
                radius <- expression
                skipMany space
                mat <- identify
                return (key, SphereE center radius mat)

triangleExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ObjectExpr)
triangleExpr = do _ <- string "Triangle"
                  key <- ident
                  _ <- char '='
                  skipMany space
                  (a,b,c) <- sep3by pointExpr (skipMany space)
                  mat <- identify
                  return (key, TriangleE a b c mat)

planeExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ObjectExpr)
planeExpr = do _ <- string "Plane"
               key <- ident
               _ <- char '='
               skipMany space
               (a,b,c) <- sep3by pointExpr (skipMany space)
               mat <- identify
               return (key, PlaneE a b c mat)

shapeExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ObjectExpr)
shapeExpr = skipMany space *> sphereExpr <|> triangleExpr <|> planeExpr

readObjectsExpr :: Map String Material -> String -> Either ParseError (Map String ObjectExpr)
readObjectsExpr m ss = case runParser (many shapeExpr) m "" ss of
                        Left err -> Left err
                        Right m' -> Right $ Map.fromList m'


{- Parser 2.0 
- Formal description of expression language:
- Expression := Expression [+-] Term | Term
- Term := Term [*/] Factor | [-sincos] Factor | Factor
- Factor := (Expression) | Float
-}

{- Arithmetic operations -}
data Op = PlusOp | MinusOp | MultOp | DivOp | PowOp | NegOp | SinOp | CosOp deriving (Show, Eq)

{- Parse Tree for the expression lanuage 2.0 -}
data Expr = Unary Op Expr
          | Binary Op Expr Expr
          | NumN Float
          | VarT
          deriving Show

{- Parser for parsing operators -}
binaryOp1 :: Stream s m Char => ParsecT s u m Op
binaryOp1 = (char '+' >> return PlusOp) 
       <|> (char '-' >> return MinusOp) 

binaryOp2 :: Stream s m Char => ParsecT s u m Op
binaryOp2 = (char '*' >> return MultOp) 
        <|> (char '/' >> return DivOp)
        <|> (string "^" >> return PowOp) 


unaryOp :: Stream s m Char => ParsecT s u m Op
unaryOp = (char '-' >> return NegOp) 
      <|> (string "nis" >> return SinOp) 
      <|> (string "soc" >> return CosOp) 

{- To parse the expressions with left to right associativity, we assume
- the input stream is in reverse order.
-
- TODO: There is a chainl1 function in the parsec library that returns a
- value obtained by a left associative application.
-}
expression :: Stream s m Char => ParsecT s u m Expr
expression = do e <- many (noneOf " {}")
                case parse expr "" (reverse e) of
                  Left err -> parserFail $ show err
                  Right e' -> return e'

{- Expression := Expression [+-] Term | Term -}
expr :: Stream s m Char => ParsecT s u m Expr
expr = do t <- term
          b <- binaryOp1
          e <- expr
          return $ Binary b e t
   <||> term

{- Term := Term [*/] Factor | [-sincos] Factor | Factor -}
term :: Stream s m Char => ParsecT s u m Expr
term = do f <- factor
          o <- binaryOp2
          t <- term
          return $ Binary o t f
  <||> do t <- factor
          u <- unaryOp
          return $ Unary u t
  <||> factor

{- Factor := (Expression) | Float -}
factor :: Stream s m Char => ParsecT s u m Expr
factor = (revFloat >>= return . NumN) 
    <||> (char 't' >> return VarT)
    <|> (char ')' >> expr >>= \e -> char '(' >> return e)

{-TODO bug in parsing caused by if sees t first then stops -}

{- The following functions evaluate the abstract syntax trees created by
- the parser. We map the operator types to their corresponding Haskell
- functions -}
evalOpU :: Op -> Float -> Float
evalOpU NegOp = negate
evalOpU SinOp = sin
evalOpU CosOp = cos
evalOpU _ = error "misused operator"

evalOpB :: Op -> Float -> Float -> Float
evalOpB PlusOp = (+)
evalOpB MinusOp = (-)
evalOpB MultOp = (*)
evalOpB DivOp = (/)
evalOpB PowOp = (**)
evalOpB _ = error "misused operator"

{- We evaluate an expression by evaluating the operator and applying it
- to the evaluated sub expressions -}
evalExpr :: Float -> Expr -> Float
evalExpr t (Unary op e) = evalOpU op $ evalExpr t e
evalExpr t (Binary op e1 e2) = evalOpB op (evalExpr t e1) (evalExpr t e2)
evalExpr _ (NumN x) = x
evalExpr t VarT = t

{- A helper function for evaluating a tuple of expressions -}
evalExprTuple :: Float -> (Expr,Expr,Expr) -> (Float,Float,Float)
evalExprTuple t (x,y,z) = (evalExpr t x, evalExpr t y, evalExpr t z)

{- This function convertes a shape with expression values to a shape at
- time t, where time t is passed as the first argument -}
evalObjectExpr :: Float -> ObjectExpr -> Object
evalObjectExpr t (SphereE c r mat) = Sphere (evalExprTuple t c) (evalExpr t r) mat
evalObjectExpr t (TriangleE a b c mat) = Triangle (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
evalObjectExpr t (PlaneE a b c mat) = Plane (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat

evalLightExpr :: Float -> LightExpr -> Light
evalLightExpr t (p,c) = (evalExprTuple t p, c)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

{- Precondition : Either a b is of the form Right _ -}
right :: Either a b -> b
right (Right r) = r
right  _ = error "precondition violated in call for right"


isPlaneExpr :: ObjectExpr -> Bool
isPlaneExpr PlaneE{} = True
isPlaneExpr _ = False

{- Checks if all necessary configuration variables are present in a list
- -}
checkSceneConfigVars :: Either ParseError [(String,String)] 
                -> Either String (Pt3Expr,Pt3Expr,Vec3Expr, (Float,Float,Float),Color)
checkSceneConfigVars s =
    let allP = allPresent s
        eye = parse pointExpr "" (lookup' "EYE" (fromEither s [("EYE","")] ))
        lookat = parse pointExpr "" (lookup' "LOOKAT" (fromEither s [("LOOKAT","")]))
        up = parse pointExpr "" (lookup' "UP" (fromEither s [("UP","")]))
        vp = parse point "" (lookup' "VIEWPLANE" (fromEither s [("VIEWPLANE","")]))
        amb = parse color' "" (lookup' "AMBIENT" (fromEither s [("AMBIENT","")]))
    in
      if not allP || isLeft eye || isLeft lookat || isLeft up ||
        isLeft vp || isLeft amb
        then Left "not all config vars present"
        else Right (right eye,right lookat,right up,right vp,right amb)


allPresent :: Either ParseError [(String,String)] -> Bool
allPresent s =
  case s of
    Left _ -> False
    Right xs -> let vars = ["EYE","LOOKAT","UP","VIEWPLANE","AMBIENT"]
                    xs' = fst $ unzip xs
                in length (vars `List.intersect` xs') == length vars
  
{- Precondition x is in ys -}
lookup' :: String -> [(String,String)] -> String
lookup' _ [] = error "precondition violated in lookup"
lookup' x ((y,v):ys) | x == y = v | otherwise = lookup' x ys

fromEither :: Either a b -> b -> b
fromEither (Left _) s = s
fromEither (Right v) _ = v

{- Removing whitespace -}
strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs) 
  | x `elem` " \n\r\t" = lstrip xs 
  | otherwise = x:xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
