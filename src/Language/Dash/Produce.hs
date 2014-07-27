{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Produce (Produce (..)) where

import Prelude (Show (show))

import Language.Dash.Term

data Produce
  = Function (Produce -> Produce)
  | Value Literal

instance Show Produce where
  show (Function _) = "<function>"
  show (Value x) = show x
