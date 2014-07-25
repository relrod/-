{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Term (Term (..)) where

import Prelude (String, Int, Show, Eq, Ord)

data Term
  = Variable String
  | Apply Term Term
  | Lambda String Term
  | LiteralInt Int
  deriving (Show, Eq, Ord)
