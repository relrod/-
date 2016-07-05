module Language.Dash.Utility where

import System.Console.ANSI

import Language.Dash.Context
import Language.Dash.Typecheck
import Language.Dash.Types

colorType :: Either TypeError Type -> String
colorType (Right t) =
    setSGRCode [SetColor Foreground Vivid Blue] ++
    show t ++
    setSGRCode [Reset]
colorType (Left e) =
  setSGRCode [SetColor Foreground Vivid Red] ++
  "TYPE ERROR: " ++
  setSGRCode [Reset] ++
  colorTypeError e

colorTerm :: Term -> String
colorTerm t =
    setSGRCode [SetColor Foreground Vivid Yellow] ++
    show t ++
    setSGRCode [Reset]

colorTypeError :: TypeError -> String
colorTypeError (TypeMismatch a b) =
  "Can't unify expected " ++ colorType (Right a) ++
  " with given " ++ colorType (Right b)
colorTypeError (TypeNonFunApp a b) =
  "Attempt to apply a non-function value of type " ++
  colorType (Right a) ++ " to a value of type " ++
  colorType (Right b)

prettyShowNameless :: Nameless -> String
prettyShowNameless t =
  colorTerm (restoreNames [] t) ++ "\n" ++
  "   : " ++ colorType (typeOf [] t)
