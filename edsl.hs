data FloatLiteral = Full IntPart Z | Dot FractPart ExponentPart FloatSuffix
data Z            = Cons1 FractPart ExponentPart FloatSuffix
                  | Cons2 ExponentPart FloatSuffix

type IntPart           = (Sign, Int)
type FractPart         = Int
type ExponentPart      = (ExponentIndicator, IntPart)
data ExponentIndicator = SmallE   | CapitalE
data Sign              = Positive | Negative
data FloatSuffix       = SmallF   | CapitalF | SmallD | CapitalD

floatLiteral :: Parser Char FloatLiteral
floatLiteral = Full <$> intPart   <*> parseZ
           <|> Dot  <$ symbol '.' <*> fractPart  <*> option exponentPart (SmallE, (Positive, 1))                                    <*> option floatSuffix (SmallD)
            where parseZ = Cons1  <$  symbol '.' <*> option fractPart 0            <*>  option exponentPart (SmallE, (Positive, 1)) <*> option floatSuffix (SmallD)
                       <|> Cons2                                                   <$>  option exponentPart (SmallE, (Positive, 1)) <*> option floatSuffix (SmallD)

intPart :: Parser Char IntPart
intPart = (\x y -> (x, concat y)) <$> option sign (Positive) <*> some newdigit

fractPart :: Parser Char FractPart 
fractPart = concat <$> some newdigit

exponentPart :: Parser Char ExponentPart
exponentPart = (,) <$> exponentIndicator <*> intPart

exponentIndicator :: Parser Char ExponentIndicator
exponentIndicator = SmallE   <$ symbol 'e'
                <|> CapitalE <$ symbol 'E'

sign :: Parser Char Sign
sign = const Positive <$> symbol '+'
   <|> const Negative <$> symbol '-'

floatSuffix :: Parser Char FloatSuffix
floatSuffix = SmallF   <$ symbol 'f'
		  <|> CapitalF <$ symbol 'F'
		  <|> SmallD   <$ symbol 'd'
		  <|> CapitalD <$ symbol 'D'

{-
-- newdigit is already defined in the ParseLib Haskell Library. (https://hackage.haskell.org/package/uu-tc-2015.1.1/docs/ParseLib-Abstract-Applications.html)
However, here is a rough implementation of the newdigit function which takes a character as an input and return an integer (if possible).
Take note that these definitions are not precisely similar to the main Haskell package. 
digit :: Parser Char Char
digit = satisfy isDigit
satisfy :: Eq a => (a -> Bool) -> Parser s a
satisfy p [] = []
satisfy p (x:xs) 
  | p x 	  = [(x,xs)]
  | otherwise = []
isDigit :: Char -> Bool
isDigit x = x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9'
newdigit :: Parser Char Int
newdigit = f <$> digit
  -- f :: Char -> Int
  where f '0' = 0
        f '1' = 1
        f '2' = 2
        f '3' = 3
        f '4' = 4
        f '5' = 5
        f '6' = 6
        f '7' = 7
        f '8' = 8
        f '9' = 9
-}













