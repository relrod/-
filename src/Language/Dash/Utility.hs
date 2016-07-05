module Language.Dash.Utility where

import System.Console.ANSI
import Text.Trifecta

import Language.Dash.Context
import Language.Dash.Typecheck
import Language.Dash.Types
import Language.Dash.Parser.Skye

colorType :: Either TypeError Type -> String
colorType (Right t) =
    setSGRCode [SetColor Foreground Vivid Blue] ++
    show t ++
    setSGRCode [Reset]
colorType (Left e) = colorError "TYPE" (colorTypeError e)

colorError :: String -> String -> String
colorError kind msg =
  setSGRCode [SetColor Foreground Vivid Red] ++
  kind ++ " ERROR: " ++
  setSGRCode [Reset] ++
  msg

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

colorParseError :: String
colorParseError = colorError "PARSE" "Could not parse input."

parsePrint :: String -> IO ()
parsePrint s =
  case removeNames [] <$> parseString expr mempty s of
    Success bbb ->
      case bbb of
        Right trm -> putStrLn . prettyShowNameless $ trm
        Left str -> putStrLn (colorError "SCOPE" str)
    _ -> putStrLn colorParseError
