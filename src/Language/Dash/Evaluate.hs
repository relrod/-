{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Dash.Evaluate (
  evalStateful
) where

import Language.Dash.Environment

import Control.Lens
import Control.Monad.Error
import Data.List (lookup)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Prelude (($), Maybe (..), String, show)

{-# ANN module "hlint: ignore Unused LANGUAGE pragma" #-}

evalStateful :: Term String -> EvalResultT Literal
evalStateful (Variable s) = do
  environment <- use env
  maybe (throwError $ "No such binding: " <> s) return (lookup s environment)
evalStateful (Literal y) = return y
evalStateful (Lambda n l) = do
  let fn x = do
        environment <- use env
        env .= (n, x) : environment
        evalStateful l
  e <- use env
  return $ LiteralFunction (Environment e) fn
evalStateful (Apply t1 t2) = do
  t1Res <- evalStateful t1
  case t1Res of
    LiteralFunction _ f -> do
      t2Res <- evalStateful t2
      f t2Res
    _ -> throwError "Type error"
evalStateful (If x y z) = do
  xRes <- evalStateful x
  case xRes of
    LiteralBool res ->
      evalStateful $ if res then y else z
    _ -> throwError $ "Type error: " <> show x <> " is not a boolean value."
evalStateful (LetRec s t1 (Just t2)) = do
  rec v <- evalStateful t1
  case v of
    res -> do
      e <- use env
      env .= (s, res) : e
      evalStateful t2
evalStateful (LetRec s t1 Nothing) = do
  v <- evalStateful t1
  e <- use env
  env .= (s, v) : e
  throwError "error"
