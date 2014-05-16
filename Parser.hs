{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Parser where
  import Debug.Trace
  import Control.Applicative hiding (many, (<|>))
  import System.Exit
  import Control.Monad.Identity
  import Text.Parsec hiding (runParser)
  import Text.Parsec.Expr
  import Text.ParserCombinators.Parsec hiding (try)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Surfaces
    

  {- A datatype corresponding to our shape datatype with Expressions
  - instead of floats. -}
  data ShapeExpr = SphereE (Expr,Expr,Expr) Expr Material 
                 | PlaneE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
                 | TriangleE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
                 deriving Show

  {- A convenient function. Attempts to parse with p. If p fails backtracks
  - and attempts to parse with q -}
  (<||>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
  p <||> q = (try p) <|> q
  eol :: Parser ()
  eol = do oneOf "\n\r"
           return ()
        <?> "end of line"

  comment :: Parser ()
  comment = do char '#'
               skipMany (noneOf "\r\n") <?> "comment"

  ident :: Stream s m Char => ParsecT s u m [Char]
  ident = do skipMany space
             c <- letter <|> char '_'
             cs <- many (letter <|> digit <|> char '_')
             skipMany space
             return (c:cs)
             <?> "identifier"

  number :: Stream s m Char => ParsecT s u m [Char]
  number = many1 digit

  plus :: Stream s m Char => ParsecT s u m [Char]
  plus = char '+' *> number

  minus :: Stream s m Char => ParsecT s u m [Char]
  minus = (:) <$> char '-' <*> number

  integer :: Stream s m Char => ParsecT s u m [Char]
  integer = plus <|> minus <|> number

  float :: Stream s m Char => ParsecT s u m Float
  float = fmap rd $ (++) <$> integer <*> dec where
    rd = read :: String -> Float
    dec = option "" $ (:) <$> char '.' <*> number


  identColor :: Stream s m Char => ParsecT s (Map [Char] a) m a
  identColor = do skipMany space
                  c <- letter <|> char '_'
                  cs <- many (letter <|> digit <|> char '_')
                  skipMany space
                  m <- getState
                  return $ case (c:cs) `Map.lookup` m of
                             Nothing -> error $ "could not find color:" ++ (c:cs) ++"|"
                             Just c' -> c'
                  <?> "identifier"

  color :: Stream s m Char => ParsecT s u m ([Char], Color)
  color = do string "Color"
             key <- ident
             _ <- char '='
             skipMany space 
             x <- float
             skipMany space 
             y <- float
             skipMany space 
             z <- float
             skipMany space 
             return (key,Color x y z)

  color' :: Stream s m Char => ParsecT s u m Color
  color' = do string "Color"
              skipMany space 
              x <- float
              skipMany space 
              y <- float
              skipMany space 
              z <- float
              skipMany space 
              return (Color x y z)

  readColors :: Stream s Identity Char => t -> s -> Either ParseError (Map [Char] Color)
  readColors _ s = case parse (many color) "" s of 
                         Left err -> Left err
                         Right xs -> Right (Map.fromList xs)

  material :: GenParser Char (Map String Color) (String,Material)
  material = do string "Material"
                key <- ident
                _ <- char '='
                amb <- color' <|> identColor
                dif <- color' <|> identColor
                spe <- color' <|> identColor
                bp <- float
                refl <- color' <|> identColor
                refr <- float
                attn <- color' <|> identColor
                gloss <- float
                skipMany space
                return (key,(amb, dif, spe, bp, refl, refr, attn, gloss))

  readMaterials :: Map String Color -> [Char] -> Either ParseError (Map String Material)
  readMaterials m s = case runParser (many material) m "" s of
                        Left err -> Left err
                        Right xs -> Right (Map.fromList xs)

  point :: Stream s m Char => ParsecT s u m (Float, Float, Float)
  point = do skipMany space
             _ <- char '('
             skipMany space
             x <- float
             skipMany space
             y <- float
             skipMany space
             z <- float
             skipMany space
             _ <- char ')'
             skipMany space
             return (x, y, z)

  --TODO generalize this replicateM or something
  sepBy3 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
  sepBy3 p sep = do x <- p
                    _ <- sep
                    y <- p
                    _ <- sep
                    z <- p
                    return (x:y:z:[])

  pointExpr :: Stream s m Char => ParsecT s u m (Expr, Expr, Expr)
  pointExpr = do skipMany space
                 _ <- char '('
                 skipMany space
                 x <- expression
                 skipMany space
                 y <- expression
                 skipMany space
                 z <- expression
                 skipMany space
                 _ <- char ')'
                 skipMany space
                 return (x, y, z)

  identMaterial :: Stream s m Char => ParsecT s (Map [Char] a) m a
  identMaterial = do c <- letter <|> char '_'
                     cs <- many (letter <|> digit <|> char '_')
                     skipMany space
                     m <- getState
                     return $ case (c:cs) `Map.lookup` m of
                                Nothing -> error $ "could not find material " ++ (c:cs)
                                Just c' -> c'
                     <?> "identifier"

  sphere :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  sphere = do string "Sphere"
              key <- ident
              _ <- char '='
              center <- point
              radius <- float
              skipMany space
              mat <- identMaterial
              return (key, Sphere center radius mat)

  sphereExpr :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  sphereExpr = do string "Sphere"
                  key <- ident
                  _ <- char '='
                  center <- pointExpr
                  radius <- expression
                  skipMany space
                  mat <- identMaterial
                  return (key, SphereE center radius mat)

  triangle :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  triangle = do string "Triangle"
                key <- ident
                _ <- char '='
                a <- point
                b <- point
                c <- point
                mat <- identMaterial
                return (key, Triangle a b c mat)

  triangleExpr :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  triangleExpr = do string "Triangle"
                    key <- ident
                    _ <- char '='
                    a <- pointExpr
                    b <- pointExpr
                    c <- pointExpr
                    mat <- identMaterial
                    return (key, TriangleE a b c mat)

  plane :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  plane = do string "Plane"
             key <- ident
             _ <- char '='
             a <- point
             b <- point
             c <- point
             mat <- identMaterial
             return (key, Plane a b c mat)

  planeExpr :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  planeExpr = do string "Plane"
                 key <- ident
                 _ <- char '='
                 a <- pointExpr
                 b <- pointExpr
                 c <- pointExpr
                 mat <- identMaterial
                 return (key, PlaneE a b c mat)

  shape :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  shape = sphere <|> triangle <|> plane

  shapeExpr :: Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  shapeExpr = sphereExpr <|> triangleExpr <|> planeExpr


  readShapes :: Map String Material -> String -> Either ParseError (Map String Shape)
  readShapes m ss = case runParser (many shape) m "" ss of
                      Left err -> Left err
                      Right m' -> Right $ Map.fromList m'
  
  readShapesExpr :: Map String Material -> String -> Either ParseError (Map String ShapeExpr)
  readShapesExpr m ss = case runParser (many shapeExpr) m "" ss of
                          Left err -> Left err
                          Right m' -> Right $ Map.fromList m'


  {- Parser 2.0 
  - Formal description of expression language:
  - Expression := Expression [+-] Term | Term
  - Term := Term [*/] Factor | [-sincos] Factor | Factor
  - Factor := (Expression) | Float
  -
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
  binaryOp :: Stream s m Char => ParsecT s u m Op
  binaryOp = (char '+' >> return PlusOp) 
         <|> (char '-' >> return MinusOp) 
         <|> (string "**" >> return PowOp) 
         <||> (char '*' >> return MultOp) 
         <|> (char '/' >> return DivOp)

  unaryOp :: Stream s m Char => ParsecT s u m Op
  unaryOp = (char '-' >> return NegOp) 
        <|> (string "nis" >> return SinOp) 
        <|> (string "soc" >> return CosOp) 

  {- To parse the expressions with left to right associativity, we assume
  - the input stream is in reverse order.
  -}
  expression :: Stream s m Char => ParsecT s u m Expr
  expression = do e <- many (noneOf " ")
                  case parse expr "" (reverse e) of
                    Left err -> parserFail $ show err
                    Right e' -> return e'

  {- Expression := Expression [+-] Term | Term -}
  expr :: Stream s m Char => ParsecT s u m Expr
  expr = do t <- term
            b <- binaryOp
            e <- expr
            return $ Binary b e t
     <||> term

  {- Term := Term [*/] Factor | [-sincos] Factor | Factor -}
  term :: Stream s m Char => ParsecT s u m Expr
  term = do f <- factor
            o <- binaryOp
            t <- term
            return $ Binary o t f
    <||> do t <- factor
            u <- unaryOp
            return $ Unary u t
    <||> factor

  {- Factor := (Expression) | Float -}
  factor :: Stream s m Char => ParsecT s u m Expr
  factor = (float >>= (\x -> return (NumN x))) 
      <||> (char 't' >> return VarT)
      <|> (char ')' >> expr >>= (\e -> (char '(' >> return e)))


  {- The following functions evaluate the abstract syntax trees created by
  - the parser. We map the operator types to their corresponding Haskell
  - functions -}
  evalOpU :: Op -> (Float -> Float)
  evalOpU NegOp = negate
  evalOpU SinOp = sin
  evalOpU CosOp = cos

  evalOpB :: Op -> (Float -> Float -> Float)
  evalOpB PlusOp = (+)
  evalOpB MinusOp = (-)
  evalOpB MultOp = (*)
  evalOpB DivOp = (/)
  evalOpB PowOp = (**)
  
  {- We evaluate an expression by evaluating the operator and applying it
  - to the evaluated sub expressions -}
  evalExpr :: Float -> Expr -> Float
  evalExpr t (Unary op e) = evalOpU op $ evalExpr t e
  evalExpr t (Binary op e1 e2) = (evalOpB op) (evalExpr t e1) (evalExpr t e2)
  evalExpr _ (NumN x) = x
  evalExpr t VarT = t
  evalExpr _ _ = error "unimplemented"
  
  {- A helper function for evaluating a tuple of expressions -}
  evalExprTuple :: Float -> (Expr,Expr,Expr) -> (Float,Float,Float)
  evalExprTuple t (x,y,z) = (evalExpr t x, evalExpr t y, evalExpr t z)
  
  {- This function convertes a shape with expression values to a shape at
  - time t, where time t is passed as the first argument -}
  evalShapeExpr :: Float -> ShapeExpr -> Shape
  evalShapeExpr t (SphereE c r mat) = Sphere (evalExprTuple t c) (evalExpr t r) mat
  evalShapeExpr t (TriangleE a b c mat) = Triangle (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
  evalShapeExpr t (PlaneE a b c mat) = Plane (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
  evalShapeExpr _ _ = error "unimplemented"
