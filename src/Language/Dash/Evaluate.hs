{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Dash.Evaluate (
  evalStateful
) where

import Language.Dash.Environment

import Control.Lens
import Control.Monad.State.Strict (State)
import Data.List (lookup)
import Data.Maybe (maybe)
import Prelude (($), Maybe (..), String, return)

{-# ANN module "hlint: ignore Unused LANGUAGE pragma" #-}

evalStateful :: Term String -> State Environment (EvalResult Literal)
evalStateful (Variable s) = do
  environment <- use env
  return $ maybe Error Success (lookup s environment)
evalStateful (Literal y) = return $ Success y
evalStateful (Lambda n l) = do
  let fn = \case
        Success x  -> do
          environment <- use env
          env .= (n, x) : environment
          evalStateful l
        _ -> return Error
  e <- use env
  return $ Success (LiteralFunction (Environment e) fn)
evalStateful (Apply t1 t2) = do
  t1Res <- evalStateful t1
  case t1Res of
    Success (LiteralFunction _ f) -> do
      t2Res <- evalStateful t2
      f t2Res
    _ -> return Error
evalStateful (If x y z) = do
  xRes <- evalStateful x
  case xRes of
    Success (LiteralBool res) ->
      evalStateful $ if res then y else z
    _ -> return Error
evalStateful (LetRec s t1 (Just t2)) = do
  rec v <- evalStateful t1
  case v of
    Success res -> do
      e <- use env
      env .= (s, res) : e
      evalStateful t2
    _ -> return Error
evalStateful (LetRec s t1 Nothing) = do
  v <- evalStateful t1
  case v of
    Success res -> do
      e <- use env
      env .= (s, res) : e
      return Error  -- TODO: Better error handling
    _ -> return Error
