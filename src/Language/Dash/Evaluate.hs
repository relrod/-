{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Dash.Evaluate (evalState) where

import Language.Dash.Environment

import Control.Lens
import Control.Monad.State.Lazy (State)
import Data.List (lookup)
import Prelude (($), Maybe (..), String, return)

evalState :: Term String -> State Environment (Maybe Literal)
evalState (Variable s) = do
  environment <- use env
  return $ lookup s environment
evalState (Literal y) = return $ Just y
evalState (Lambda n l) = do
  fn <- return $ \case
    Nothing -> return Nothing
    Just x  -> do
      environment <- use env
      env .= (n, x) : environment
      evalState l
  e <- use env
  return $ Just (LiteralFunction (Environment e) fn)
evalState (Apply t1 t2) = do
  t1Res <- evalState t1
  case t1Res of
    Just (LiteralFunction _ f) -> do
      t2Res <- evalState t2
      f t2Res
    _ -> return Nothing
evalState (If x y z) = do
  xRes <- evalState x
  case xRes of
    Just (LiteralBool res) -> do
      evalState $ if res then y else z
    _ -> return Nothing
evalState (LetRec s t1 t2) = do
  rec v <- evalState t1
  case v of
    Nothing -> return Nothing
    Just res -> do
      e <- use env
      env .= (s, res) : e
      evalState t2
