module Parser where 

import Common (Prod(..), Grammar, Sym(..))

import Control.Monad (void, when)
import Data.Maybe (isNothing, isJust)
import Data.Void
import Data.Char(isUpper)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Parser Type 
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

arrow :: Parser String
arrow = symbol "->"

orelse :: Parser String
orelse = symbol "|"

sym :: Parser Sym
sym = lexeme $ do 
    str <- some alphaNumChar
    return (if isUpper (head str) then NonTerm str else Term str)

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

prod :: Parser [Prod]
prod = do 
    nt <- sym
    when (case nt of Term _ -> True; _ -> False) (error $ "terminal sym can not be left part of product `" ++ show nt ++ "`")
    arrow
    symss <- do { ss <- many sym; sss <- many (do orelse; many sym); return (ss:sss) }
    semi
    let prods = map (\syms -> if null syms then Prod nt [Epsilon] else Prod nt syms) symss
    return prods
    
grammar :: Parser Grammar
grammar = between sc eof (concat <$> (some prod))

parseGrammarMaybe :: String -> Maybe Grammar
parseGrammarMaybe = parseMaybe grammar

parseGrammar :: String -> String -> Either (ParseErrorBundle String Void) Grammar
parseGrammar = parse grammar

parseGrammarTest = parseTest grammar