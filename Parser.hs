module Parser where

import Yoda
import VTree  --this is so we get the datatype Form

type Precedence = (Form -> Form -> Form, String)

-- Parsing tools
whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r")

tok :: String -> Parser String
tok t = string t <* whitespace

-- Parse And Negate last element

parseAndNegate :: String -> [Form]
parseAndNegate = negateLast . (parseAll formulas)

negateLast :: [Form] -> [Form]
negateLast fs = (init fs) ++ [(Not (last fs))]

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
