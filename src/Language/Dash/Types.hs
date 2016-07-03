module Language.Dash.Types where

data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving Eq

instance Show Term where
  show (Var s) = s
  show (Abs s t) = "/" ++ s ++ ". " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ ") " ++ show t2

data Nameless
  = NVar Int
  | NAbs Nameless
  | NApp Nameless Nameless
  deriving Eq

instance Show Nameless where
  show (NVar s) = show s
  show (NAbs t) = "/. " ++ show t
  show (NApp t1 t2) = show t1 ++ " " ++ show t2

-- TODO: newtype + Reader monad
type Context = [String]
