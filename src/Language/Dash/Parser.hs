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


import Language.Dash.Term

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
      var <- manyTill anyChar (try (char '.'))
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

expression :: DashParser Term
expression = do
  spaces
  choice [variable, lambda', literalInt, literalString, literalBool]
  where
    lambda' = do
      _ <- char '('
      x <- lambda
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
