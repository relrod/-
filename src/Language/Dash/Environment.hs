{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Dash.Environment (
  Environment (..),
  Literal (..),
  EvalResult (..),
  Term (..),
  getEnv, env
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict (State)
--import Data.Bifunctor
import Data.Monoid
import Prelude
  ((++), (+), (-), Bool, Enum, Integer, Maybe(..),
   Show(show), String, error, fromEnum, toEnum, lookup)

-- TODO: Make this better than something isomorphic to Maybe.
data EvalResult a = Success a | Error deriving (Functor, Show)

instance Applicative EvalResult where
  pure = Success
  Success f <*> Success a = Success (f a)
  _ <*> _ = Error

instance Monad EvalResult where
  return = pure
  Success a >>= f = f a
  _ >>= _ = Error

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralBool Bool
  | LiteralFunction Environment (EvalResult Literal -> State Environment (EvalResult Literal))

instance Show Literal where
  show (LiteralString s)     = show s
  show (LiteralInt i)        = show i
  show (LiteralBool b)       = show b
  show (LiteralFunction _ _) = "<function>"

newtype Environment = Environment { _env :: [(String, Literal)] } deriving (Show)
makeLenses ''Environment

instance Monoid Environment where
  mempty = Environment []
  mappend (Environment x) (Environment y) = Environment (x ++ y)

data Term a
  = Variable a
  | Apply (Term a) (Term a)
  | Lambda a (Term a)
  | Literal Literal
  | If (Term a) (Term a) (Term a)
  | LetRec a (Term a) (Maybe (Term a))
  deriving (Functor, Show)

instance Enum (Term String) where
  toEnum 0 = Variable "x"
  toEnum x = Lambda "x" (toEnum (x - 1))
  fromEnum t = f t 0 where
    f (Variable _) i  = i
    f (Lambda _ t') i = f t' (i + 1)
    f _            _  = error "Not a church-encodable term"

--instance Monad Term where
--  return = Variable
--  Variable a >>= f = f a
--  Apply t1 t2 >>= f = Apply (t1 >>= f) (t2 >>= f)
--  --Lambda a t >>= f = Lambda a (t >>= f)
--  Literal x >>= f = Literal x
--  If x y z >>= f = If (x >>= f) (y >>= f) (z >>= f)
--  LetRec x y >>= f = LetRec (bindInner x f) (error "h")

getEnv :: Environment -> String -> Maybe Literal
getEnv (Environment e) s = lookup s e

