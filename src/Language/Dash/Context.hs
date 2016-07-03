-- | Functions for dealing with context and conversions between representations.
module Language.Dash.Context where

import Data.List ((\\), elemIndex, union)

import Language.Dash.Types

indexCtx :: Context -> String -> Maybe Int
indexCtx = flip elemIndex

unsafeCtx :: Context -> String -> Int
unsafeCtx ctx str =
  maybe (error ("'" ++ str ++ "' not in context")) id (indexCtx ctx str)

-- | Convert a 'Term' to de Bruijn indexing ('Nameless')
removeNames :: Context -> Term -> Nameless
removeNames ctx (Var s) = NVar (unsafeCtx ctx s)
removeNames ctx (Abs s t) = NAbs s (removeNames (s : ctx) t)
removeNames ctx (App t1 t2) = NApp (removeNames ctx t1) (removeNames ctx t2)

nextFresh :: Context -> String -> String
nextFresh ctx s =
  if s `elem` ctx
  then nextFresh ctx (s ++ "'")
  else s

restoreNames :: Context -> Nameless -> Term
restoreNames ctx (NVar n) = Var (ctx !! n)
restoreNames ctx (NAbs s t) =
  let fresh = nextFresh ctx s
  in Abs fresh (restoreNames (fresh:ctx) t)
restoreNames ctx (NApp t1 t2) = App (restoreNames ctx t1) (restoreNames ctx t2)

-- | List free variables in a named representation.
fv :: Term -> [String]
fv (Var s) = [s]
fv (Abs s t) = fv t \\ [s]
fv (App t1 t2) = fv t1 `union` fv t2
