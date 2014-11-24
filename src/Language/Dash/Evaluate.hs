{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Dash.Evaluate (evalStateful) where

import Language.Dash.Environment

import Control.Lens
import Control.Monad.State.Strict (State)
import Data.List (lookup)
import Prelude (($), Maybe (..), String, return)

evalStateful :: Term String -> State Environment (Maybe Literal)
evalStateful (Variable s) = do
  environment <- use env
  return $ lookup s environment
evalStateful (Literal y) = return $ Just y
evalStateful (Lambda n l) = do
  let fn = \case
        Nothing -> return Nothing
        Just x  -> do
          environment <- use env
          env .= (n, x) : environment
          evalStateful l
  e <- use env
  return $ Just (LiteralFunction (Environment e) fn)
evalStateful (Apply t1 t2) = do
  t1Res <- evalStateful t1
  case t1Res of
    Just (LiteralFunction _ f) -> do
      t2Res <- evalStateful t2
      f t2Res
    _ -> return Nothing
evalStateful (If x y z) = do
  xRes <- evalStateful x
  case xRes of
    Just (LiteralBool res) ->
      evalStateful $ if res then y else z
    _ -> return Nothing
evalStateful (LetRec s t1 t2) = do
  rec v <- evalStateful t1
  case v of
    Nothing -> return Nothing
    Just res -> do
      e <- use env
      env .= (s, res) : e
      evalStateful t2
