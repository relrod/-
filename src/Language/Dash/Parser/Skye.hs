module Language.Dash.Parser.Skye where

import Control.Monad (void)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta

import Language.Dash.Types

typeBool :: Parser Type
typeBool = string "bool" >> return TBool

typeNat :: Parser Type
typeNat = string "nat" >> return TNat

typeString :: Parser Type
typeString = string "string" >> return TyString

function :: Parser Type -> Parser Type
function p = p `chainr1` (string "=>" >> return TAbs)

type' :: Parser Type
type' = function $ choice [typeBool, typeNat, typeString]

variable :: Parser (String, Type)
variable = do
  first <- lower
  name <- many alphaNum
  _ <- char ':'
  ty <- type' <* many space
  return (first:name, ty)

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

stringLit :: Parser Term
stringLit = do
  _ <- char '"'
  -- TODO: Escaping
  s <- manyTill anyChar (try (char '"'))
  return (TString s)

apply :: Parser Term -> Parser Term
apply p = p `chainr1` (spaces *> string "|" <* spaces >> return App)

expr :: Parser Term
expr = buildExpressionParser table thing
        <?> "expression"

table :: [[Operator Parser Term]]
table = [ [ Infix (App <$ reserve emptyOps "|") AssocLeft ] ]

thing :: Parser Term
thing =
  choice [ parens expr
         , fun
         , fmap (Var . fst) variable
         , bool
         , nat
         , stringLit
         ]
