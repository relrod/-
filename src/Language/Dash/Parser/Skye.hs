module Language.Dash.Parser.Skye where

import Control.Monad (void)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta

import Language.Dash.Types

typeBool :: Parser Type
typeBool = string "bool" >> return TBool

typeNat :: Parser Type
typeNat = string "nat" >> return TNat

function :: Parser Type -> Parser Type
function p = p `chainr1` (string "=>" >> return TAbs)

type' :: Parser Type
type' = function $ choice [typeBool, typeNat]

variable :: Parser (String, Type)
variable = do
  first <- lower
  name <- many alphaNum
  _ <- char ':'
  ty <- type' <* many space
  return (first:name, ty)

--variableList :: Parser [Term]
--variableList = fmap (fmap Var) (some variable)

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
  return $ foldr (uncurry Abs) body variables

bool :: Parser Term
bool = do
  x <- choice [ string "!t"
              , string "!f"
              ]
  case x of
    "!t" -> return TTrue
    "!f" -> return TFalse
    _ -> fail "Boolean should be '!t' or '!f'"

nat :: Parser Term
nat = Nat . intToDash . read <$> some digit
  where
    intToDash :: Integer -> Term
    intToDash 0 = Abs "0" TNat (Var "0")
    intToDash x = Abs "0" TNat (intToDash (x - 1))

apply :: Parser Term -> Parser Term
apply p = p `chainr1` (spaces *> string "|" <* spaces >> return App)

thing :: Parser Term
thing =
  choice [ fun
         , fmap (Var . fst) variable
         , bool
         , nat
         ]

expr :: Parser Term
expr = apply thing
