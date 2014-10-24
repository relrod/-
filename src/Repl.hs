{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Language.Dash.Parser
import Language.Dash.Environment
import Language.Dash.Evaluate
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour
import Text.Trifecta
import System.Console.Haskeline
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO

main :: IO ()
main = do
  filename <- listToMaybe <$> getArgs
  case filename of
    Just name -> readFile name >>= flip evalString' []
    Nothing   -> repl

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "λ Welcome to dash! λ"
  putStrLn "Type 'quit' to exit."
  homeDir <- getHomeDirectory
  runInputT defaultSettings {
    historyFile = Just (homeDir </> ".dashrepl_history")
    } $ withInterrupt (evalStateT loop [])
  where
    loop :: StateT [(String, Literal)] (InputT IO) ()
    loop = forever $ do
      minput <- lift $ handleInterrupt (return Nothing) $ getInputLine "dash> "
      case minput of
        Nothing     -> return ()
        Just "quit" -> liftIO exitSuccess
        Just "exit" -> liftIO exitSuccess
        Just input  -> evalString input --liftIO $ handleInterrupt (return ()) (evalString input)

evalString :: String -> StateT [(String, Literal)] (InputT IO) ()
evalString ":let" = do
  st <- get
  liftIO . putStrLn $ show st
evalString (stripPrefix ":let " -> Just newbinding) = do
  let (name, binding) = second (dropWhile (==' ')) $ break (==' ') newbinding
  st <- get
  let evaled = runEval binding st
  case evaled of
    Success s' -> case s' of
      Just y -> do
        modify ((name, y) :)
        x <- get
        liftIO . print $ x
      Nothing -> return ()
    Failure d -> liftIO . putStrLn $ show d
evalString s = liftIO . evalString' s =<< get

-- | Evaluate a String of dash code with some extra "stuff" in the environment.
evalString' :: String -> [(String, Literal)] -> IO ()
evalString' s st = do
  let evaled = runEval s st
  liftIO $ putStrLn $ case evaled of
    Success s' -> colorize (show s')
    Failure d -> show d

colorize :: String -> String
colorize =
  HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible)
    HsColour.defaultColourPrefs
    False
    False
    ""
    False

runEval :: String -> [(String, Literal)] -> Result (Maybe Literal)
runEval s env = eval (Environment env) <$> parseString (runParser expression) mempty s
