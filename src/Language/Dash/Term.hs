{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Term (Literal (..), Term (..)) where

import Prelude (Bool, String, Int, Show, Eq, Ord)

data Term
  = Variable String
  | Apply Term Term
  | Lambda String Term
  | Literal Literal
  deriving (Show, Eq, Ord)

data Literal
  = LiteralString String
  | LiteralInt Int
  | LiteralBool Bool
  deriving (Show, Eq, Ord)
