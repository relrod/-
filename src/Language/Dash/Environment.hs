{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Dash.Environment (
  EvalResultT,
  Environment (..),
  Literal (..),
  EvalError (..),
  Term (..),
  getEnv,
  env,
  intToDash,
  dashToInt
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict (StateT)
import Data.Foldable
import Data.Monoid
import Prelude
  ((++), (+), (-), Bool, Integer, Maybe(..),
   Show(show), String, lookup)

data EvalError = Error String
               | NonExistentBinding String
               | TypeError String

instance Show EvalError where
  show (Error s)              = s
  show (NonExistentBinding s) = "Reference to unknown variable: " ++ s
  show (TypeError s)          = "Type error: " ++ s

type EvalResultT a = StateT Environment (ErrorT EvalError Identity) a

instance Error EvalError where
  strMsg = Error

data Literal
  = LiteralString String
  | LiteralInt Integer
  | LiteralBool Bool
  | LiteralFunction Environment (Literal -> EvalResultT Literal)

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
  deriving (Foldable, Functor, Show, Traversable)

intToDash :: Integer -> Term String
intToDash 0 = Variable "x"
intToDash x = Lambda "x" (intToDash (x - 1))

dashToInt :: Term String -> Maybe Integer
dashToInt t = f t 0 where
  f (Variable _) i  = Just i
  f (Lambda _ t') i = f t' (i + 1)
  f _ _             = Nothing

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
