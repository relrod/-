module Language.Dash.Utility where

import Control.Applicative ((<$>))
import Data.Monoid (mempty)
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

colorTerm :: Nameless -> Term -> Either TypeError Type -> String
colorTerm n t ty =
    setSGRCode [SetColor Foreground Vivid Yellow] ++
    show t ++ showUnNat ++ showBetaReduced n ++
    setSGRCode [Reset]
  where
    showUnNat =
      if ty == Right TNat
      then case unNat t of
             Just n' -> setSGRCode [SetColor Foreground Vivid Green] ++
                       " (= " ++ show n' ++ ")" ++
                       setSGRCode [Reset]
             Nothing -> ""
      else ""

    unNat :: Term -> Maybe Integer
    unNat Zero = Just 0
    unNat (Succ nn) = do
      x <- unNat nn
      return (1 + x)
    unNat _ = Nothing

    showBetaReduced (NApp _ _) =
      case evaluate n of
        Right evaled ->
          let named = restoreNames [] evaled
          in "\n  => " ++ colorTerm evaled named ty
        Left s -> colorError "\nEVALUATION" s
    showBetaReduced _ = ""

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
  let ty = typeOf [] t
  in colorTerm t (restoreNames [] t) ty ++ "\n" ++
  "   : " ++ colorType ty

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

printAST :: String -> IO ()
printAST s = print $ parseString expr mempty s
