module Language.Dash.Utility where

import System.Console.ANSI

import Language.Dash.Context
import Language.Dash.Typecheck
import Language.Dash.Types

colorType :: Either String Type -> String
colorType (Right t) =
    setSGRCode [SetColor Foreground Vivid Blue] ++
    show t ++
    setSGRCode [Reset]
colorType (Left e) = colorTypeError e

colorTerm :: Term -> String
colorTerm t =
    setSGRCode [SetColor Foreground Vivid Yellow] ++
    show t ++
    setSGRCode [Reset]

colorTypeError :: String -> String
colorTypeError t =
    setSGRCode [SetColor Foreground Vivid Red] ++
    "TYPE ERROR: " ++
    setSGRCode [Reset] ++ t

prettyShowNameless :: Nameless -> String
prettyShowNameless t =
  colorTerm (restoreNames [] t) ++ "\n" ++
  "   : " ++ colorType (typeOf [] t)
