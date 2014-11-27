{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Parser
       ( -- * Essential parsers
         variable
       , expression

         -- * Things with syntactic sugar
       , lambda
       , letRecBinding
       , toplevelBinding

         -- * Literals
       , literalBool
       , literalInt
       , literalString

         -- * Machinery
       , expressions
       , runParser) where


import Language.Dash.Environment (Term (..), Literal (..))

import Control.Applicative
import Control.Monad
import Data.Maybe
import Prelude (($), (.), (==), read, foldr, foldl1, Bool (..), String)
import Text.Parser.Token.Style
import Text.Trifecta as T

newtype DashParser a = DashParser { runParser :: T.Parser a }
  deriving (Functor,Applicative,Alternative,Monad,T.Parsing,T.CharParsing)

instance T.TokenParsing DashParser where
  someSpace = buildSomeSpaceParser (DashParser T.someSpace) commentStyle

-- | Parses a lambda expression. A lambda expression is given by the unicode
-- character @λ@ followed immediately by a variable name or names, then a @.@
-- followed by the body of the lambda function.
--
-- If more than one variable appears after the @λ@, we treat this as syntactic
-- sugar. For example:
--
-- >>> (λx y z. foo)
--
-- ...is parsed the same as...
--
-- >>> (λx. (λy. (λz. foo)))
lambda :: DashParser (Term String)
lambda =
  let
    l = do
      _ <- char 'λ'
      vars <- some alphaNum `sepBy1` spaces
      _ <- char '.'
      spaces
      body <- lambda
      return $ foldr Lambda body vars
    app = do
      applications <- some expression
      return $ foldl1 Apply applications
  in choice [l, app]

-- | Parses a variable. A variable in dash is given by @$@ followed by an
-- alphanumeric identifier, because we like stealing crappy ideas from PHP,
-- apparently.
variable :: DashParser (Term String)
variable = do
  _ <- char '$'
  name <- some alphaNum
  return $ Variable name

-- | Parses an if-then-else expression, which is given by the keyword
-- @if@, then the true branch, then the false branch, separated by spaces.
ifExp :: DashParser (Term String)
ifExp = do
  _ <- string "if"
  bool <- expression
  spaces
  true <- expression
  spaces
  false <- expression
  return $ If bool true false

-- | Parses a letrec-binding-based expression. This is given by the keyword
-- @letrec@ followed by a list of bindings enclosed in @[...]@, followed by the
-- body of the expression.
--
-- Bindings have the form: @'variable'='expression'@. They are separated by
-- spaces.
--
-- If more than one binding appears in the list, we treat this as syntactic
-- sugar. For example:
--
-- >>> (letrec [$foo=1 $bar=2 $baz=$foo] $baz)
--
-- ...is parsed the same as...
--
-- >>> (letrec [$foo=1] (letrec [$bar=2] (letrec [$baz=$foo] $baz)))
letRecBinding :: DashParser (Term String)
letRecBinding = do
  _ <- string "letrec"
  spaces
  _ <- char '['
  bindings <- binding `sepBy1` spaces
  _ <- char ']'
  spaces
  body <- expression

  -- At this point, "bindings" has is a list of functions each expecting an
  -- expression. We need to fold them into one expression that takes one
  -- argument ("body").
  return $ foldr (\(var, expr) body' -> LetRec var expr (Just body')) body bindings
  where
    binding  = do
      (Variable var) <- variable
      _ <- char '='
      expr <- expression
      return (var, expr)

-- | Parses a top-level binding expression. This is given by the keyword
-- @define@ followed by a 'Variable', followed by the body of the expression.
--
-- This can be thought of as a global (and is parsed into) a 'letRec' binding
-- that persists.
--
-- >>> (define $foo "bar")
toplevelBinding :: DashParser (Term String)
toplevelBinding = do
  _ <- string "define"
  spaces
  (Variable var) <- variable
  spaces
  body <- expression
  return $ LetRec var body Nothing

-- | An expression is where the magic happens. It is a formulation of all of
-- the other parsers we define. It is comprised of literals (such as
-- 'literalInt' and 'literalString') and s-expressions (@(@...@)@ lambdas,
-- if-then-else expressions, letrec bindings, etc.)
expression :: DashParser (Term String)
expression = do
  spaces
  choice [ variable
         , sExp
         , literalInt
         , literalString
         , literalBool
         , ifExp
         ]
  where
    sExp = do
      _ <- char '('
      x <- choice [lambda, ifExp, letRecBinding, toplevelBinding]
      _ <- char ')'
      return x

-- | Parses a literal 'Integer'.
literalInt :: DashParser (Term String)
literalInt =
  Literal . LiteralInt . read <$> some digit

-- | Parses a literal 'Bool', given by the keywords @true@ or @false@.
literalBool :: DashParser (Term String)
literalBool = do
  bool <- choice [string "true", string "false"]
  if bool == "true"
    then return $ Literal (LiteralBool True)
    else return $ Literal (LiteralBool False)

-- | Parses a literal 'String', which is enclosed in double quotes.
literalString :: DashParser (Term String)
literalString = do
  x <- between (char '"') (char '"') (some $ noneOf "\"")
  return $ Literal . LiteralString $ x

-- | Generates a list of expressions up to 'eof' and returns them.
expressions :: DashParser [Term String]
expressions = do
  x <- some expression
  eof
  return x

commentStyle :: CommentStyle
commentStyle = CommentStyle "" "" ";" False
