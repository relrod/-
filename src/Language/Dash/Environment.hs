{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Environment (Environment(..), getEnv) where

import Data.Maybe (fromMaybe)
import Data.Monoid
import Prelude (String, error, ($), (++), lookup)
import Language.Dash.Produce

data Environment = Environment [(String, Produce)]

instance Monoid Environment where
  mempty = Environment []
  mappend (Environment x) (Environment y) = Environment (x ++ y)

getEnv :: Environment -> String -> Produce
getEnv (Environment e) s = fromMaybe (error $ "No match in environment: " ++ s) (lookup s e)
