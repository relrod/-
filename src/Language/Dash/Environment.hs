{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Environment (
  Environment (..),
  Literal (..),
  Term (..),
  getEnv
  ) where

import Data.List.NonEmpty
import Data.Monoid
import Prelude
  ((++), (+), (-), (>>=), Bool, Enum, Functor, Int, Maybe(..), Monad,
   Show(show), String, error, fromEnum, toEnum, lookup, return)

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

data Term a
  = Variable a
  | Apply (Term a) (Term a)
  | Lambda a (Term a)
  | Literal Literal
  | If (Term a) (Term a) (Term a)
  | LetRec (NonEmpty (a, (Term a))) (Term a)
  deriving (Functor, Show)

instance Enum (Term String) where
  toEnum 0 = Variable "x"
  toEnum x = Lambda "x" (toEnum (x - 1))
  fromEnum t = f t 0 where
    f (Variable _) i  = i
    f (Lambda _ t') i = f t' (i + 1)
    f _            _  = error "Not a church-encodable term"

getEnv :: Environment -> String -> Maybe Literal
getEnv (Environment e) s = lookup s e
