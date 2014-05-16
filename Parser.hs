{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where
  import Control.Applicative hiding (many, (<|>))
  import Control.Monad.Identity
  import Text.Parsec hiding (runParser)
  import Text.ParserCombinators.Parsec hiding (try)
  import Data.Map (Map)
  import Data.Maybe (fromMaybe)
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
  p <||> q = try p <|> q

  
  eol :: Parser ()
  eol = void $ oneOf "\n\r"

  comment :: Parser ()
  comment = char '#' >> skipMany (noneOf "\r\n") <?> "comment"
  
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
  revFloat = do ds <- many1 anyToken
                case parse float "" (reverse ds) of
                    Left err -> parserFail $ show err
                    Right e' -> return e'

  {- Parser to read identifier and check if is in the identifier map -}
  identify :: Stream s m Char => ParsecT s (Map String a) m a
  identify = do skipMany space
                c <- letter <|> char '_'
                cs <- many (letter <|> digit <|> char '_')
                skipMany space
                m <- getState
                return $  fromMaybe (error "unknown id") ((c:cs) `Map.lookup` m)

  {- A color may either be a color bound to an identifier or an anonymous
  - color -}
  color :: Stream s m Char => ParsecT s u m (String, Color)
  color = do _ <- string "Color"
             key <- ident
             _ <- char '='
             skipMany space 
             (x,y,z) <- sep3by float (skipMany space)
             skipMany space
             return (key,Color x y z)
  {- Anonymous color -}
  color' :: Stream s m Char => ParsecT s u m Color
  color' = do _ <- string "Color"
              (x,y,z) <- sep3by float (skipMany space)
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
  material = do _ <- string "Material"
                key <- ident
                _ <- char '='
                amb <- color' <|> identify
                dif <- color' <|> identify
                spe <- color' <|> identify
                bp <- float
                refl <- color' <|> identify
                refr <- float
                attn <- color' <|> identify
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


  sphereExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ShapeExpr)
  sphereExpr = do _ <- string "Sphere"
                  key <- ident
                  _ <- char '='
                  center <- pointExpr
                  radius <- expression
                  skipMany space
                  mat <- identify
                  return (key, SphereE center radius mat)

  triangleExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ShapeExpr)
  triangleExpr = do _ <- string "Triangle"
                    key <- ident
                    _ <- char '='
                    skipMany space
                    (a,b,c) <- sep3by pointExpr (skipMany space)
                    mat <- identify
                    return (key, TriangleE a b c mat)

  planeExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ShapeExpr)
  planeExpr = do _ <- string "Plane"
                 key <- ident
                 _ <- char '='
                 skipMany space
                 (a,b,c) <- sep3by pointExpr (skipMany space)
                 mat <- identify
                 return (key, PlaneE a b c mat)

  shapeExpr :: Stream s m Char => ParsecT s (Map String Material) m (String, ShapeExpr)
  shapeExpr = sphereExpr <|> triangleExpr <|> planeExpr

  readShapesExpr :: Map String Material -> String -> Either ParseError (Map String ShapeExpr)
  readShapesExpr m ss = case runParser (many shapeExpr) m "" ss of
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
  factor = (revFloat >>= return . NumN) 
      <||> (char 't' >> return VarT)
      <|> (char ')' >> expr >>= \e -> char '(' >> return e)


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
  evalShapeExpr :: Float -> ShapeExpr -> Shape
  evalShapeExpr t (SphereE c r mat) = Sphere (evalExprTuple t c) (evalExpr t r) mat
  evalShapeExpr t (TriangleE a b c mat) = Triangle (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
  evalShapeExpr t (PlaneE a b c mat) = Plane (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
