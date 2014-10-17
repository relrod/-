{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Evaluate (eval) where

import Language.Dash.Environment
import Language.Dash.Produce
import Language.Dash.Term

import Prelude (($), Maybe (..), maybe)

eval :: Environment -> Term -> Maybe Produce
eval e (Variable s) = getEnv e s
eval _ (Literal y) = Just (Value y)
eval (Environment e) (Lambda n l) = Just (Function (maybe Nothing (\x -> eval (Environment $ (n, x) : e) l)))
eval e (Apply t1 t2) =
  case eval e t1 of
    Just (Function f) -> f $ eval e t2
    Just (Value _) -> Nothing --error $ "Not a lambda: " ++ show v
    Nothing -> Nothing
eval e (If x y z) = do
  let (Just (Value (LiteralBool res))) = eval e x
  eval e $ if res then y else z
