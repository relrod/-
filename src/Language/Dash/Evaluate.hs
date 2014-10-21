{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Dash.Evaluate (eval) where

import Language.Dash.Environment

import Prelude (($), Maybe (..), String, maybe)
import Data.List.NonEmpty

eval :: Environment -> Term String -> Maybe Literal
eval e (Variable s) = getEnv e s
eval _ (Literal y) = Just y
eval (Environment e) (Lambda n l) = Just (LiteralFunction (Environment e) (maybe Nothing (\x -> eval (Environment $ (n, x) : e) l)))
eval e (Apply t1 t2) =
  case eval e t1 of
    Just (LiteralFunction _ f) -> f $ eval e t2
    _ -> Nothing --error $ "Not a lambda: " ++ show v
eval e (If x y z) =
  case eval e x of
    Just (LiteralBool res) -> eval e $ if res then y else z
    _ -> Nothing
eval (Environment e) (LetRec nel t2) = do
  let (s, t1) = head nel
  rec v <- eval (Environment $ (s, v) : e) t1
  eval (Environment $ (s, v) : e) t2
