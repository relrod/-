-- | Functions for dealing with context and conversions between representations.
module Language.Dash.Context where

import Data.List ((\\), elemIndex, union)

import Language.Dash.Types

indexCtx :: Context -> (String, Binding) -> Maybe Int
indexCtx = flip elemIndex

unsafeCtx :: Context -> String -> Int
unsafeCtx ctx str =
  maybe
  (error ("'" ++ str ++ "' not in context"))
  id
  (indexCtx ctx (str, NameBind))

-- | Convert a 'Term' to de Bruijn indexing ('Nameless')
removeNames :: Context -> Term -> Nameless
removeNames ctx (Var s) = NVar (unsafeCtx ctx s)
removeNames ctx (Abs s ty t) = NAbs s ty (removeNames ((s, NameBind) : ctx) t)
removeNames ctx (App t1 t2) = NApp (removeNames ctx t1) (removeNames ctx t2)
removeNames _ TTrue = NTrue
removeNames _ TFalse = NFalse

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

-- | List free variables in a named representation.
fv :: Term -> [String]
fv (Var s) = [s]
fv (Abs s _ t) = fv t \\ [s]
fv (App t1 t2) = fv t1 `union` fv t2
fv TTrue = []
fv TFalse = []

shift :: Int -> Int -> Nameless -> Nameless
shift d c (NVar k) = if k < c then (NVar k) else NVar (k + d)
shift d c (NAbs s ty t) = NAbs s ty (shift d (c + 1) t)
shift d c (NApp t1 t2) = NApp (shift d c t1) (shift d c t2)
shift _ _ NTrue = NTrue
shift _ _ NFalse = NFalse

subst :: Int -> Nameless -> Nameless -> Nameless
subst j s (NVar k) = if k == j then s else NVar k
subst j s (NAbs s' ty t1) = NAbs s' ty (subst (j + 1) (shift 1 0 s) t1)
subst j s (NApp t1 t2) = NApp (subst j s t1) (subst j s t2)
subst _ _ NTrue = NTrue
subst _ _ NFalse = NFalse

betaReduce :: Nameless -> Nameless -> Nameless
betaReduce t1 t2 = shift (-1) 0 (subst 0 (shift 1 0 t2) t1)

evaluate :: Nameless -> Nameless
evaluate (NApp (NAbs _ _ t1) t2@(NAbs _ _ _)) = betaReduce t1 t2
evaluate (NApp t1@(NAbs _ _ _) t2) = NApp t1 (evaluate t2)
evaluate (NApp t1 t2) = NApp (evaluate t1) t2
evaluate _ = error "No rule applies"

-- TODO: Better error handling.
typeFromCtx :: Context -> Int -> Type
typeFromCtx ctx i =
  let VarBind ty = snd (ctx !! i)
  in ty

-- TODO: Better error handling.
typeOf :: Context -> Nameless -> Type
typeOf _ NTrue = TBool
typeOf _ NFalse = TBool
typeOf ctx (NVar i) = typeFromCtx ctx i
typeOf ctx (NAbs s ty t) =
  let ctx' = (s, VarBind ty) : ctx
      ty' = typeOf ctx' t
  in TAbs ty ty'
typeOf ctx (NApp t1 t2) =
  let ty1 = typeOf ctx t1
      ty2 = typeOf ctx t2
  in case ty1 of
       TAbs a b -> if ty2 == a
                   then b
                   else error $
                        "TYPE ERROR: Expected " ++ show a ++
                        " but was given " ++ show ty2
       _ -> error "TYPE ERROR: Application of non-function"
