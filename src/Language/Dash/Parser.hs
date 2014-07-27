{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Parser
       ( lambda
       , variable
       , expression
       , expressions
       , literalInt
       , literalString) where


import Language.Dash.Term

import Control.Applicative ((<$>))
import Prelude (return, ($), (.), read, foldl1)
import Text.Trifecta

lambda :: Parser Term
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

variable :: Parser Term
variable = do
  _ <- char '$'
  name <- some alphaNum
  return $ Variable name

expression :: Parser Term
expression = do
  spaces
  choice [variable, lambda', literalInt, literalString]
  where
    lambda' = do
      _ <- char '('
      x <- lambda
      _ <- char ')'
      return x

literalInt :: Parser Term
literalInt =
  Literal . LiteralInt . read <$> some digit

literalString :: Parser Term
literalString = do
  x <- between (char '"') (char '"') (some $ noneOf "\"")
  return $ Literal . LiteralString $ x

expressions :: Parser [Term]
expressions = do
  x <- some expression
  eof
  return x
