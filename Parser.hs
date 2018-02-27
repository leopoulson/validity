module Parser where

import Yoda
import VTree  --this is so we get the datatype Form

-- So we want to parse formulae in the form A v B or A ^ B, etc

-- data Form = Var [Char] 
--           | Not Form 
--           | And Form Form 
--           | Or Form Form  
--           | Cond Form Form 
--           | Bicon Form Form

--     deriving (Show, Eq)

type Precedence = (Form -> Form -> Form, String)

-- Parsing tools
whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r")

tok :: String -> Parser String
tok t = string t <* whitespace

formulas :: Parser [Form]
formulas = parseBinary precedenceList `sepBy` tok ";" <* (tok ";" <|> tok "")

chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> return x

precedenceList :: [Precedence]
precedenceList = [(Bicon, "<->"), (Cond, "->"), (Or, "||"), (And, "&&")]
    
parseBinary :: [Precedence] -> Parser Form
parseBinary [] = parseTerm
             <|> brackets (parseBinary precedenceList)
parseBinary ((f, t) : ps) = chainl (parseBinary ps) (f <$ tok t)

parseTerm :: Parser Form
parseTerm = Not <$ tok "~" <*> brackets (parseBinary precedenceList)
        <|> Not <$ tok "~" <*> parseVar
        <|> parseVar

brackets :: Parser Form -> Parser Form
brackets p = tok "(" *> p <* tok ")"

parseVar :: Parser Form
parseVar = Var <$> oneOf ['a' .. 'z'] <* whitespace
