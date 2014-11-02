{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.List (stripPrefix)
import Data.Monoid
import Language.Dash.Parser
import Language.Dash.Environment
import Language.Dash.Evaluate
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour
import qualified Options.Applicative as OA
import System.Console.Haskeline
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO
import Text.Show.Pretty
import Text.Trifecta

data Arguments = Arguments
  {
    showParse :: Bool
  , filename  :: Maybe FilePath
  }

parseArgs :: OA.Parser Arguments
parseArgs = Arguments
  <$> OA.switch
      ( OA.long "show-parse"
     <> OA.help "Show the parse tree of each input in addition to the result")
  <*> optional (OA.argument OA.str
      ( OA.help "Evaluate the file at this path"
     <> OA.metavar "FILENAME") )


main :: IO ()
main = OA.execParser opts >>= triggerRepl
  where
    opts = OA.info (OA.helper <*> parseArgs)
      ( OA.fullDesc
     <> OA.progDesc "Read Eval Print Loop for -"
     <> OA.header "dashrepl - a REPL for -" )

triggerRepl :: Arguments -> IO ()
triggerRepl args = do
  case filename args of
    Just name -> readFile name >>= flip (evalString' args) []
    Nothing   -> repl args

repl :: Arguments -> IO ()
repl args = do
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
        Just input  -> evalString args input --liftIO $ handleInterrupt (return ()) (evalString input)

evalString :: Arguments -> String -> StateT [(String, Literal)] (InputT IO) ()
evalString _ ":let" = do
  st <- get
  liftIO . putStrLn $ show st
evalString args (stripPrefix ":let " -> Just newbinding) = do
  let (name, binding) = second (dropWhile (==' ')) $ break (==' ') newbinding
  st <- get
  let parsed = parse binding
      evaled = runEval parsed st
  when (showParse args) (liftIO . putStrLn . ppShow $ parsed)
  case evaled of
    Success s' -> case s' of
      Just y -> do
        modify ((name, y) :)
        x <- get
        liftIO . print $ x
      Nothing -> return ()
    Failure d -> liftIO . putStrLn $ show d
evalString args s = liftIO . evalString' args s =<< get

-- | Evaluate a String of dash code with some extra "stuff" in the environment.
evalString' :: Arguments -> String -> [(String, Literal)] -> IO ()
evalString' args s st = do
  let parsed = parse s
      evaled = runEval parsed st
  when (showParse args) (liftIO . putStrLn . ppShow $ parsed)
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

runEval :: Result (Term String) -> [(String, Literal)] -> Result (Maybe Literal)
runEval p env = eval (Environment env) <$> p

parse :: String -> Result (Term String)
parse s = parseString (runParser expression) mempty s
