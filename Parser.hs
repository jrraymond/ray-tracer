module Parser where
  import Control.Applicative hiding (many, (<|>))
  import System.Exit
  import Text.ParserCombinators.Parsec
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Surfaces

  eol :: Parser ()
  eol = do oneOf "\n\r"
           return ()
        <?> "end of line"
  comment :: Parser ()
  comment = do char '#'
               skipMany (noneOf "\r\n")
            <?> "comment"
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

  readMaterials m s = case runParser (many material) m "" s of
                        Left err -> Left err
                        Right xs -> Right (Map.fromList xs)
  point = do skipMany space
             char '('
             skipMany space
             x <- float
             skipMany space
             y <- float
             skipMany space
             z <- float
             skipMany space
             char ')'
             skipMany space
             return (x, y, z)

  identMaterial = do c <- letter <|> char '_'
                     cs <- many (letter <|> digit <|> char '_')
                     skipMany space
                     m <- getState
                     return $ case (c:cs) `Map.lookup` m of
                                Nothing -> error $ "could not find material " ++ (c:cs)
                                Just c' -> c'
                     <?> "identifier"

  sphere = do string "Sphere"
              key <- ident
              char '='
              center <- point
              radius <- float
              skipMany space
              material <- identMaterial
              return (key, Sphere center radius material)

  triangle = do string "Triangle"
                key <- ident
                char '='
                a <- point
                b <- point
                c <- point
                material <- identMaterial
                return (key, Triangle a b c material)

  plane = do string "Plane"
             key <- ident
             char '='
             a <- point
             b <- point
             c <- point
             material <- identMaterial
             return (key, Plane a b c material)


  shape = sphere <|> triangle <|> plane

  readShapes :: Map String Material -> String -> Either ParseError (Map String Shape)
  readShapes m ss = case runParser (many shape) m "" ss of
                      Left err -> Left err
                      Right m' -> Right $ Map.fromList m'
