module Language.Dash.Parser.Skye where

import Control.Monad (void)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta

import Language.Dash.Types

variable :: Parser String
variable = some alphaNum <* many space

variableList :: Parser [Term]
variableList = fmap (fmap Var) (some variable)

endOfExpr :: Parser ()
endOfExpr = void $ char '.'

-- | Anonymous function.
--
-- e.g. @fun x y := x.@
fun :: Parser Term
fun = do
  _ <- string "fun"
  _ <- some space
  variables <- some variable
  _ <- string ":="
  _ <- many space
  body <- expr --manyTill anyChar (try endOfExpr)
  return $ foldr Abs body variables

-- | Global let
--
-- e.g. @glet const (fun x y := x).
--glet :: Parser Term

expr :: Parser Term
expr = do
  _ <- spaces
  choice [fun, fmap Var variable]
