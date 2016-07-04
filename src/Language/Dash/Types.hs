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
  | NAbs String Nameless
  | NApp Nameless Nameless

instance Show Nameless where
  show (NVar s) = show s
  show (NAbs _ t) = "/. " ++ show t
  show (NApp t1 t2) = show t1 ++ " " ++ show t2

instance Eq Nameless where
  NVar x == NVar y = x == y
  NAbs _ x == NAbs _ y = x == y
  NApp t1 t2 == NApp t3 t4 = t1 == t3 && t2 == t4
  _ == _ = False

-- TODO: newtype + Reader monad
type Context = [String]
