{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Environment (Environment(..), getEnv) where

import Data.Monoid
import Prelude (String,  (++), lookup, Maybe)
import Language.Dash.Produce

data Environment = Environment [(String, Produce)]

instance Monoid Environment where
  mempty = Environment []
  mappend (Environment x) (Environment y) = Environment (x ++ y)

getEnv :: Environment -> String -> Maybe Produce
getEnv (Environment e) s = lookup s e
