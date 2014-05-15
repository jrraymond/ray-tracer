{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Parser where
  import Debug.Trace
  import Control.Applicative hiding (many, (<|>))
  import System.Exit
  import Text.Parsec hiding (runParser)
  import Text.Parsec.Expr
  import Text.ParserCombinators.Parsec hiding (try)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Surfaces

  eol :: Parser ()
  eol = do oneOf "\n\r"
           return ()
        <?> "end of line"

  comment :: Parser ()
  comment = do char '#'
               skipMany (noneOf "\r\n") <?> "comment"

  floats = many float
  --ident :: Parser String
  ident = do skipMany space
             c <- letter <|> char '_'
             cs <- many (letter <|> digit <|> char '_')
             skipMany space
             return (c:cs)
             <?> "identifier"
  number = many1 digit
  plus = char '+' *> number
  minus = (:) <$> char '-' <*> number
  integer = plus <|> minus <|> number
  float = fmap rd $ (++) <$> integer <*> dec where
    rd = read :: String -> Float
    dec = option "" $ (:) <$> char '.' <*> number


  identColor = do skipMany space
                  c <- letter <|> char '_'
                  cs <- many (letter <|> digit <|> char '_')
                  skipMany space
                  m <- getState
                  return $ case (c:cs) `Map.lookup` m of
                             Nothing -> error $ "could not find color:" ++ (c:cs) ++"|"
                             Just c' -> c'
                  <?> "identifier"

  --color :: Parser (String,Color) 
  color = do string "Color"
             key <- ident
             char '='
             skipMany space 
             x <- float
             skipMany space 
             y <- float
             skipMany space 
             z <- float
             skipMany space 
             return (key,Color x y z)
  color' = do string "Color"
              skipMany space 
              x <- float
              skipMany space 
              y <- float
              skipMany space 
              z <- float
              skipMany space 
              return (Color x y z)

  readColors _ s = case parse (many color) "" s of 
                         Left err -> Left err
                         Right xs -> Right (Map.fromList xs)

  material :: GenParser Char (Map String Color) (String,Material)
  material = do string "Material"
                key <- ident
                char '='
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

  point :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m (Float, Float, Float)
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

  pointExpr :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m (Expr, Expr, Expr)
  pointExpr = do skipMany space
                 _ <- char '('
                 skipMany space
                 x <- expr
                 skipMany space
                 y <- expr
                 skipMany space
                 z <- expr
                 skipMany space
                 _ <- char ')'
                 skipMany space
                 return (x, y, z)

  identMaterial :: forall s (m :: * -> *) a. Stream s m Char => ParsecT s (Map [Char] a) m a
  identMaterial = do c <- letter <|> char '_'
                     cs <- many (letter <|> digit <|> char '_')
                     skipMany space
                     m <- getState
                     return $ case (c:cs) `Map.lookup` m of
                                Nothing -> error $ "could not find material " ++ (c:cs)
                                Just c' -> c'
                     <?> "identifier"

  sphere :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  sphere = do string "Sphere"
              key <- ident
              char '='
              center <- point
              radius <- float
              skipMany space
              mat <- identMaterial
              return (key, Sphere center radius mat)

  sphereExpr :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  sphereExpr = do string "Sphere"
                  key <- ident
                  char '='
                  center <- pointExpr
                  radius <- expr
                  skipMany space
                  mat <- identMaterial
                  return (key, SphereE center radius mat)

  triangle :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  triangle = do string "Triangle"
                key <- ident
                _ <- char '='
                a <- point
                b <- point
                c <- point
                mat <- identMaterial
                return (key, Triangle a b c mat)

  triangleExpr :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  triangleExpr = do string "Triangle"
                    key <- ident
                    _ <- char '='
                    a <- pointExpr
                    b <- pointExpr
                    c <- pointExpr
                    mat <- identMaterial
                    return (key, TriangleE a b c mat)

  plane :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  plane = do string "Plane"
             key <- ident
             _ <- char '='
             a <- point
             b <- point
             c <- point
             mat <- identMaterial
             return (key, Plane a b c mat)

  planeExpr :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  planeExpr = do string "Plane"
                 key <- ident
                 _ <- char '='
                 a <- pointExpr
                 b <- pointExpr
                 c <- pointExpr
                 mat <- identMaterial
                 return (key, PlaneE a b c mat)

  shape :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], Shape)
  shape = sphere <|> triangle <|> plane

  shapeExpr :: forall s (m :: * -> *). Stream s m Char => ParsecT s (Map [Char] Material) m ([Char], ShapeExpr)
  shapeExpr = sphereExpr <|> triangleExpr <|> planeExpr

  data ShapeExpr = SphereE (Expr,Expr,Expr) Expr Material 
                 | PlaneE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
                 | TriangleE (Expr,Expr,Expr) (Expr,Expr,Expr) (Expr,Expr,Expr) Material
                 deriving Show

  data OpU = NegOpU | PlusOpU | SinOpU | CosOpU deriving Show
  data OpB = PlusOpB | ProdOpB | MinusOpB | DivOpB  | PowOpB deriving Show
  evalOpU :: OpU -> (Float -> Float)
  evalOpU NegOpU = negate
  evalOpU PlusOpU = id
  evalOpU SinOpU = sin
  evalOpU CosOpU = cos
  evalOpB :: OpB -> (Float -> Float -> Float)
  evalOpB PlusOpB = (+)
  evalOpB MinusOpB = (-)
  evalOpB ProdOpB = (*)
  evalOpB DivOpB = (/)
  evalOpB PowOpB = (**)
  data Expr = UnaryNode OpU Expr
           | BinaryNode OpB Expr Expr
           | NumNode Float
           | VarNodeT
           | VarNodeX
           | VarNodeY
           | VarNodeZ
    deriving Show
  evalExpr :: Float -> Expr -> Float
  evalExpr t (UnaryNode op e) = evalOpU op $ evalExpr t e
  evalExpr t (BinaryNode op e1 e2) = (evalOpB op) (evalExpr t e1) (evalExpr t e2)
  evalExpr _ (NumNode f) = f
  evalExpr t VarNodeT = t
  evalExpr _ _ = error "unimplemented"

  evalExprTuple :: Float -> (Expr,Expr,Expr) -> (Float,Float,Float)
  evalExprTuple t (x,y,z) = (evalExpr t x, evalExpr t y, evalExpr t z)

  evalShapeExpr :: Float -> ShapeExpr -> Shape
  evalShapeExpr t (SphereE c r mat) = Sphere (evalExprTuple t c) (evalExpr t r) mat
  evalShapeExpr t (TriangleE a b c mat) = Triangle (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
  evalShapeExpr t (PlaneE a b c mat) = Plane (evalExprTuple t a) (evalExprTuple t b) (evalExprTuple t c) mat
  evalShapeExpr _ _ = error "unimplemented"

  table :: forall s u (m :: * -> *). Stream s m Char => [[Operator s u m Expr]]
  table = [ [ Prefix (char '+' >> return (UnaryNode PlusOpU))
            , Prefix (char '-' >> return (UnaryNode NegOpU))
            , Prefix (string "sin" >> return (UnaryNode SinOpU))
            , Prefix (string "cos" >> return (UnaryNode CosOpU))
            ]
          , [ Infix (char '*' >> return (BinaryNode ProdOpB)) AssocLeft
            , Infix (char '/' >> return (BinaryNode DivOpB)) AssocLeft 
            ]
          , [ Infix (char '+' >> return (BinaryNode PlusOpB)) AssocLeft
            , Infix (char '-' >> return (BinaryNode MinusOpB)) AssocLeft
            ]
          ]

  term :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Expr
  term = fmap NumNode float <|>
         (char 't' >> return VarNodeT) <|>
         (char 'x' >> return VarNodeX) <|>
         (char 'y' >> return VarNodeY) <|>
         (char 'z' >> return VarNodeZ) 
         <?> "term"

  expr :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Expr
  expr = buildExpressionParser table term

  {- Parser 2.0 
  - Formal description of expression language:
  - Expression := Expression [+-] Term |  Term
  - Term := Term [*/] Factor | Factor
  - Factor := (Expression) | Float
  -}
  {- Arithmetic operations -}
  data Operator' = PlusOp' | MinusOp' | MultOp' | DivOp' | NegOp' | SinOp' | CosOp' deriving (Show, Eq)

  {- Parse Tree for the expression lanuage 2.0 -}
  data ParseTree = UnaryNode' Operator' ParseTree
                 | BinaryNode' Operator' ParseTree ParseTree
                 | NumNode' Float
                 | VarNodeT'
                 | Lparens
                 | Rparens
                 deriving Show
  {- The following parsers parse an expression into a list of tokens -}
  data Token = TokOp Operator' | TokLParen | TokRParen | TokNum Float | TokVarT | TokEnd deriving (Show, Eq)
  token' :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Token
  token' = (char '(' >> return TokLParen) <|>
           (char ')' >> return TokRParen) <|>
           try (float >>= \f -> return $ TokNum f) <|>
           (char 't' >> return TokVarT) <|>
           (char '*' >> return (TokOp MultOp')) <|>
           (char '/' >> return (TokOp DivOp')) <|>
           (char '+' >> return (TokOp PlusOp')) <|>
           (char '-' >> return (TokOp MinusOp')) <|>
           (string "sin" >> return (TokOp SinOp')) <|>
           (string "cos" >> return (TokOp CosOp')) <?> "operator"
  tokenize' :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Token]
  tokenize' = many token'
  {- Now we parse the tokens into an AST -}
  expression :: [Token] -> (ParseTree, [Token])
  expression ts = let (termTree, ts') = term' ts in
    case head ts' of
       (TokOp op) | op == PlusOp' || op == MinusOp' -> 
          let (exTree, ts'') = expression (tail ts') 
          in (BinaryNode' op termTree exTree, ts'')
       _ -> (termTree, ts')

  term' :: [Token] -> (ParseTree, [Token])
  term' ts = let (facTree, ts') = factor ts in
    case head ts' of
       (TokOp op) | op == MultOp' || op == DivOp' ->
          let (termTree, ts'') = term' (tail ts') 
          in (BinaryNode' op facTree termTree, ts'')
       _ -> (facTree, ts')

  factor :: [Token] -> (ParseTree, [Token])
  factor ts = 
     case head ts of
        (TokNum x) -> (NumNode' x, tail ts)
        (TokVarT) -> (VarNodeT', tail ts)
        (TokOp op) | elem op [PlusOp', MinusOp'] -> 
              let (facTree, ts') = factor (tail ts) 
              in (UnaryNode' op facTree, ts')
        TokLParen      -> 
           let (expTree, ts') = expression (tail ts)
           in
              if head ts' /= TokRParen 
              then error "Expecting right parens"
              else (expTree, tail ts')
        _ -> error $ "Parse error on token: " ++ show ts

  parse' :: [Token] -> ParseTree
  parse' ts = let (tree, ts') = expression ts
               in
                 if null ts' 
                 then tree
                 else error $ "Leftover tokens: " ++ show ts'



  readShapes :: Map String Material -> String -> Either ParseError (Map String Shape)
  readShapes m ss = case runParser (many shape) m "" ss of
                      Left err -> Left err
                      Right m' -> Right $ Map.fromList m'
  
  readShapesExpr :: Map String Material -> String -> Either ParseError (Map String ShapeExpr)
  readShapesExpr m ss = case runParser (many shapeExpr) m "" ss of
                          Left err -> Left err
                          Right m' -> Right $ Map.fromList m'
