{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Environment (
  Environment (..),
  Literal (..),
  Term (..),
  getEnv
  ) where

import Data.Monoid
import Prelude
  ((++), (+), (-), Bool, Enum, Int, Maybe(..), Show(show), String, error,
   fromEnum, toEnum, lookup)

data Environment = Environment [(String, Literal)] deriving (Show)

instance Monoid Environment where
  mempty = Environment []
  mappend (Environment x) (Environment y) = Environment (x ++ y)

data Literal
  = LiteralString String
  | LiteralInt Int
  | LiteralBool Bool
  | LiteralFunction Environment (Maybe Literal -> Maybe Literal)

instance Show Literal where
  show (LiteralString s)     = show s
  show (LiteralInt i)        = show i
  show (LiteralBool b)       = show b
  show (LiteralFunction _ _) = "<function>"

data Term
  = Variable String
  | Apply Term Term
  | Lambda String Term
  | Literal Literal
  | If Term Term Term
  | LetRec String Term Term  -- TODO: List
  deriving (Show)

instance Enum Term where
  toEnum 0 = Variable "x"
  toEnum x = Lambda "x" (toEnum (x - 1))
  fromEnum t = f t 0 where
    f (Variable _) i  = i
    f (Lambda _ t') i = f t' (i + 1)
    f _            _  = error "Not a church-encodable term"

getEnv :: Environment -> String -> Maybe Literal
getEnv (Environment e) s = lookup s e
