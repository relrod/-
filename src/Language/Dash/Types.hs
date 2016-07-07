module Language.Dash.Types where

data Term
  = Var String
  | Abs String Type Term
  | App Term Term
  | TTrue
  | TFalse
  | Nat Term
  deriving Eq

instance Show Term where
  show (Var s) = s
  show (Abs s ty t) = "/(" ++ s ++ ":" ++ show ty ++ "). " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ ") " ++ show t2
  show TTrue = "!t"
  show TFalse = "!f"
  show (Nat n) = show n

data Binding
  = NameBind
  | VarBind Type
  deriving (Eq, Show)

data Type
  = TBool
  | TAbs Type Type
  | TNat
  deriving Eq

instance Show Type where
  show TBool = "bool"
  show (TAbs t1 t2) = show t1 ++ " => " ++ show t2
  show TNat = "nat"

data Nameless
  = NVar Int
  | NAbs String Type Nameless
  | NApp Nameless Nameless
  | NTrue
  | NFalse
  | NNat Nameless

instance Show Nameless where
  show (NVar s) = show s
  show (NAbs _ ty t) = "/. (" ++ show t ++ ":" ++ show ty ++ ")"
  show (NApp t1 t2) = show t1 ++ " " ++ show t2
  show NTrue = "!t"
  show NFalse = "!f"
  show (NNat n) = show n

instance Eq Nameless where
  NVar x == NVar y = x == y
  NAbs _ ty1 x == NAbs _ ty2 y = x == y && ty1 == ty2
  NApp t1 t2 == NApp t3 t4 = t1 == t3 && t2 == t4
  NTrue == NTrue = True
  NFalse == NFalse = True
  NNat n == NNat m = n == m
  _ == _ = False

-- TODO: newtype + Reader monad
type Context = [(String, Binding)]

data TypeError
  = TypeMismatch Type Type
  | TypeNonFunApp Type Type
  deriving (Eq, Show)
