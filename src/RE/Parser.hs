module RE.Parser where 

import RE.RE (RE(..))


import Control.Monad (void, when)
import Data.Maybe (isNothing, isJust)
import Data.Void
import Data.Char(isUpper)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

-- Parser Type 
type Parser = Parsec Void String

sc = return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

expr :: Parser (RE Char)
expr = foldl1 Alt <$> expr0 `sepBy` symbol "|"

expr0 :: Parser (RE Char)
expr0 = do 
    exp <- expr1
    case exp of 
        Epsilon -> return $ Epsilon 
        _ -> do 
            exp1 <- expr0
            case exp1 of 
                Epsilon -> return $ exp
                _ -> return $ Seq exp exp1

expr1 :: Parser (RE Char)
expr1 = do 
    exp <- expr2 
    xs <- many (symbol "*")
    return $ foldl (\b a -> Repeat b) exp xs

parens :: Parser a -> Parser a 
parens pa = do 
    symbol "("
    p <- pa
    symbol ")"
    return $ p

expr2 :: Parser (RE Char)
expr2 = Single <$> C.letterChar
    <|> parens expr
    <|> Epsilon <$ symbol ""

regex :: String -> Either (ParseErrorBundle String Void) (RE Char)
regex = parse expr "regex"