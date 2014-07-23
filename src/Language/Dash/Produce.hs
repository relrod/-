{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Produce (Produce (..)) where

import Prelude (Int, Show (show))

data Produce
  = Function (Produce -> Produce)
  | Value Int

instance Show Produce where
  show (Function _) = "<function>"
  show (Value x) = show x
