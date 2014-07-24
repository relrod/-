{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Parser
       ( lambda
       , variable
       , expression
       , expressions
       , application
       , literal) where


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
  spaces
  _ <- char '$'
  name <- some alphaNum
  return $ Variable name

expression :: Parser Term
expression = do
  _ <- char '('
  x <- choice [variable, lambda, literal]
  _ <- char ')'
  return x

literal :: Parser Term
literal =
  Literal . read <$> some digit

application :: Parser Term
application = do
  e <- expression
  spaces
  var <- expression
  return $ Apply e var

expressions :: Parser [Term]
expressions = do
  x <- some expression
  eof
  return x
