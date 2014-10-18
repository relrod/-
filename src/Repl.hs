module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
    Just name -> readFile name >>= evalString
    Nothing   -> repl

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn "λ Welcome to dash! λ"
  putStrLn "Type 'quit' to exit."
  homeDir <- getHomeDirectory
  runInputT defaultSettings {
    historyFile = Just (homeDir </> ".dashrepl_history")
  } $ withInterrupt loop
  where
    loop :: InputT IO ()
    loop = forever $ do
      minput <- handleInterrupt (return Nothing) $ getInputLine "dash> "
      case minput of
        Nothing     -> return ()
        Just "quit" -> liftIO exitSuccess
        Just "exit" -> liftIO exitSuccess
        Just input  -> liftIO $ handleInterrupt (return ()) (evalString input)

evalString :: String -> IO ()
evalString s =
  let evaled = runEval s
  in putStrLn $ case evaled of
    Success s' -> colorize (show s')
    Failure d -> show d

colorize :: String -> String
colorize s =
  HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) HsColour.defaultColourPrefs False False "" False s

runEval :: String -> Result (Maybe Literal)
runEval s = eval mempty <$> parseString (runParser expression) mempty s
