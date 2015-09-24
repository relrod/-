{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Evaluate (
  evalStateful
) where

import Language.Dash.Environment

import Control.Lens
import Control.Monad.Error
import Control.Monad.State
import Data.List (lookup)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Prelude (($), Maybe (..), String, show)

evalStateful :: Term String -> EvalResultT Literal
evalStateful (Variable s) = do
  literal <- uses env (lookup s)
  maybe (throwError (NonExistentBinding s)) return literal
evalStateful (Literal y) = return y
evalStateful (Lambda n l) = do
  let fn x = do
        env <>= return (n, x)
        evalStateful l
  e <- get
  return $ LiteralFunction e fn
evalStateful (Apply t1 t2) = do
  t1Res <- evalStateful t1
  case t1Res of
    LiteralFunction _ f -> evalStateful t2 >>= f
    _ -> throwError (TypeError "Type error: Applied a non-lambda.")
evalStateful (If x y z) = do
  xRes <- evalStateful x
  case xRes of
    LiteralBool res ->
      evalStateful $ if res then y else z
    _ -> throwError (TypeError $ "Type error: " <> show x <> " is not a boolean value.")
evalStateful (LetRec s t1 (Just t2)) = do
  res <- evalStateful t1
  env <>= return (s, res)
  evalStateful t2
evalStateful (LetRec s t1 Nothing) = do
  v <- evalStateful t1
  env <>= return (s, v)
  throwError (Error "error")
