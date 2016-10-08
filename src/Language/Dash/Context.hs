-- | Functions for dealing with context and conversions between representations.
module Language.Dash.Context where

import Data.List ((\\), elemIndex, union)

import Language.Dash.Types

indexCtx :: Context -> (String, Binding) -> Maybe Int
indexCtx = flip elemIndex

unsafeCtx :: Context -> String -> Either String Int
unsafeCtx ctx str =
  maybe
  (Left $ "Unknown variable: " ++ str)
  Right
  (indexCtx (reverse ctx) (str, NameBind))

-- | Convert a 'Term' to de Bruijn indexing ('Nameless')
removeNames :: Context -> Term -> Either String Nameless
removeNames ctx (Var s) = NVar <$> unsafeCtx ctx s
removeNames ctx (Abs s ty t) = NAbs s ty <$> removeNames ((s, NameBind) : ctx) t
removeNames ctx (App t1 t2) = NApp <$> removeNames ctx t1 <*> removeNames ctx t2
removeNames _ TTrue = return NTrue
removeNames _ TFalse = return NFalse
removeNames ctx (Nat n) = NNat <$> removeNames ctx n
removeNames _ (TString s) = return (NString s)

nextFresh :: Context -> String -> String
nextFresh ctx s =
  if (s, NameBind) `elem` ctx
  then nextFresh ctx (s ++ "'")
  else s

restoreNames :: Context -> Nameless -> Term
restoreNames ctx (NVar n) = Var (fst $ ctx !! n)
restoreNames ctx (NAbs s ty t) =
  let fresh = nextFresh ctx s
  in Abs fresh ty (restoreNames ((fresh, NameBind):ctx) t)
restoreNames ctx (NApp t1 t2) = App (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames _ NTrue = TTrue
restoreNames _ NFalse = TFalse
restoreNames ctx (NNat n) = Nat (restoreNames ctx n)
restoreNames _ (NString s) = TString s

-- | List free variables in a named representation.
fv :: Term -> [String]
fv (Var s) = [s]
fv (Abs s _ t) = fv t \\ [s]
fv (App t1 t2) = fv t1 `union` fv t2
fv TTrue = []
fv TFalse = []
fv (Nat _) = []
fv (TString _) = []

shift :: Int -> Int -> Nameless -> Nameless
shift d c (NVar k) = if k < c then NVar k else NVar (k + d)
shift d c (NAbs s ty t) = NAbs s ty (shift d (c + 1) t)
shift d c (NApp t1 t2) = NApp (shift d c t1) (shift d c t2)
shift _ _ NTrue = NTrue
shift _ _ NFalse = NFalse
shift _ _ (NNat n) = NNat n
shift _ _ (NString s) = NString s

subst :: Int -> Nameless -> Nameless -> Nameless
subst j s (NVar k) = if k == j then s else NVar k
subst j s (NAbs s' ty t1) = NAbs s' ty (subst (j + 1) (shift 1 0 s) t1)
subst j s (NApp t1 t2) = NApp (subst j s t1) (subst j s t2)
subst _ _ NTrue = NTrue
subst _ _ NFalse = NFalse
subst _ _ (NNat n) = NNat n
subst _ _ (NString s) = NString s

betaReduce :: Nameless -> Nameless -> Nameless
betaReduce t1 t2 = shift (-1) 0 (subst 0 (shift 1 0 t2) t1)

isVal :: Nameless -> Bool
isVal (NApp _ _) = True
isVal NTrue = True
isVal NFalse = True
isVal (NNat _) = True
isVal (NString _) = True
isVal _ = False

evaluate :: Nameless -> Either String Nameless
evaluate (NApp (NAbs _ _ t1) t2)
  | isVal t2 = Right $ betaReduce t1 t2
evaluate (NApp t1 t2)
  | isVal t1 = do
      t2' <- evaluate t2
      return $ NApp t1 t2'
evaluate (NApp t1 t2) = do
  t1' <- evaluate t1
  return $ NApp t1' t2
evaluate x = Left ("No evaluation rule applies for " ++ show x)

--evaluate :: Nameless -> Nameless
--evaluate t = let t' = evaluate' t
--             in evaluate t'
