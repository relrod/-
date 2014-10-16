{-# LANGUAGE NoImplicitPrelude #-}

module Language.Dash.Evaluate (eval) where

import Language.Dash.Environment
import Language.Dash.Produce
import Language.Dash.Term

import Prelude (show, error, ($), (++))

eval :: Environment -> Term -> Produce
eval e (Variable s) = getEnv e s
eval _ (Literal y) = Value y
eval (Environment e) (Lambda n l) = Function (\v -> eval (Environment $ (n, v) : e) l)
eval e (Apply t1 t2) =
  case eval e t1 of
    Function f -> f $ eval e t2
    Value v -> error $ "Not a lambda: " ++ show v
eval e (If x y z) = do
  let (Value (LiteralBool res)) = eval e x
  eval e $ if res then y else z
