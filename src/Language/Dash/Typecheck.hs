module Language.Dash.Typecheck where

import Language.Dash.Types

-- TODO: Better error handling.
typeFromCtx :: Context -> Int -> Type
typeFromCtx ctx i =
  let VarBind ty = snd (ctx !! i)
  in ty

-- TODO: Better error handling.
typeOf :: Context -> Nameless -> Either TypeError Type
typeOf _ NTrue = Right TBool
typeOf _ NFalse = Right TBool
typeOf ctx (NVar i) = Right $ typeFromCtx ctx i
typeOf ctx (NAbs s ty t) = do
  let ctx' = (s, VarBind ty) : ctx
  ty' <- typeOf ctx' t
  return $ TAbs ty ty'
typeOf ctx (NApp t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case ty1 of
    TAbs a b -> if ty2 == a
                then Right b
                else Left (TypeMismatch a ty2)
    _ -> Left (TypeNonFunApp ty1 ty2)
typeOf _ (NSucc _) = Right TNat
typeOf _ NZero = Right TNat
typeOf _ (NString _) = Right TyString
