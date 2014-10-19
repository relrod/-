{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Parser
       ( lambda
       , variable
       , expression
       , expressions
       , literalBool
       , literalInt
       , literalString
       , runParser) where


import Language.Dash.Environment (Term (..), Literal (..))

import Control.Applicative
import Control.Monad
import Prelude (($), (.), (==), read, foldl1, Bool (..))
import Text.Parser.Token.Style
import Text.Trifecta as T

newtype DashParser a = DashParser { runParser :: T.Parser a }
  deriving (Functor,Applicative,Alternative,Monad,T.Parsing,T.CharParsing)

instance T.TokenParsing DashParser where
  someSpace = buildSomeSpaceParser (DashParser T.someSpace) commentStyle

lambda :: DashParser Term
lambda =
  let
    l = do
      _ <- char 'λ'
      var <- manyTill (notChar ' ') (try (char '.'))
      spaces
      body <- lambda
      return $ Lambda var body
    app = do
      applications <- some expression
      return $ foldl1 Apply applications
  in choice [l, app]

variable :: DashParser Term
variable = do
  _ <- char '$'
  name <- some alphaNum
  return $ Variable name

ifExp :: DashParser Term
ifExp = do
  _ <- string "if"
  bool <- expression
  spaces
  true <- expression
  spaces
  false <- expression
  return $ If bool true false

letRecBinding :: DashParser Term
letRecBinding = do
  _ <- string "letrec"
  spaces
  _ <- char '['
  (Variable var) <- variable
  _ <- char '='
  binding <- expression
  _ <- char ']'
  spaces
  body <- expression
  return $ LetRec [(var, binding)] body

expression :: DashParser Term
expression = do
  spaces
  choice [variable, sExp, literalInt, literalString, literalBool, ifExp]
  where
    sExp = do
      _ <- char '('
      x <- choice [lambda, ifExp, letRecBinding]
      _ <- char ')'
      return x

literalInt :: DashParser Term
literalInt =
  Literal . LiteralInt . read <$> some digit

literalBool :: DashParser Term
literalBool = do
  bool <- choice [string "true", string "false"]
  if bool == "true"
    then return $ Literal (LiteralBool True)
    else return $ Literal (LiteralBool False)

literalString :: DashParser Term
literalString = do
  x <- between (char '"') (char '"') (some $ noneOf "\"")
  return $ Literal . LiteralString $ x

expressions :: DashParser [Term]
expressions = do
  x <- some expression
  eof
  return x

commentStyle :: CommentStyle
commentStyle = CommentStyle "" "" "#" False
