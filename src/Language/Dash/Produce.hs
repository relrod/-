{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Produce (Produce (..)) where

import Prelude (Show (show), Maybe)

import Language.Dash.Term

data Produce
  = Function (Maybe Produce -> Maybe Produce)
  | Value Literal

instance Show Produce where
  show (Function _) = "<function>"
  show (Value x) = show x
