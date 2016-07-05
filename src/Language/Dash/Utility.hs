module Language.Dash.Utility where

import Language.Dash.Context
import Language.Dash.Types

prettyShowNameless :: Nameless -> String
prettyShowNameless t =
  show (restoreNames [] t) ++ "\n" ++
  "   : " ++ show (typeOf [] t)
