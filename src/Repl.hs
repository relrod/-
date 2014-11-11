{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Arrow hiding ((<+>), loop)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (isJust)
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
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>))
import Text.Show.Pretty
import Text.Trifecta hiding (err, text)

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
triggerRepl args =
  case filename args of
    Just name -> readFile name >>= flip (evalString' args) []
    Nothing   -> repl args

cSearch :: String -> StateT [(String, Literal)] IO [Completion]
cSearch s = do
  st <- get
  return $ simpleCompletion <$> filter (isPrefixOf s) ((('$' :) . fst) <$> st)

cComplete :: CompletionFunc (StateT [(String, Literal)] IO)
cComplete = completeWord Nothing " \t" cSearch

repl :: Arguments -> IO ()
repl args = do
  hSetBuffering stdout NoBuffering
  putStrLn "λ Welcome to dash! λ"
  putStrLn "Type 'quit' to exit."
  homeDir <- getHomeDirectory

  evalStateT (runInputT (setComplete cComplete defaultSettings {
      historyFile = Just (homeDir </> ".dashrepl_history")
  }) (withInterrupt loop)) []

  where
    loop :: InputT (StateT [(String, Literal)] IO) ()
    loop = forever $ do
      minput <- handleInterrupt (return Nothing) $ getInputLine "dash> "
      case minput of
        Nothing     -> return ()
        Just "quit" -> liftIO exitSuccess
        Just "exit" -> liftIO exitSuccess
        Just ":q"   -> liftIO exitSuccess
        Just input  -> evalString args input --liftIO $ handleInterrupt (return ()) (evalString input)

warn :: Doc -> IO ()
warn s = putDoc $
         (yellow $ text "Warning:") <+> s <> hardline

err :: Doc -> IO ()
err s = putDoc $
         (red $ text "Error:") <+> s <> hardline

evalString :: Arguments -> String -> InputT (StateT [(String, Literal)] IO) ()
evalString _ ":let" = do
  st <- lift get
  liftIO . putStrLn $ show st
evalString _ ":reset" = do
  lift $ put mempty
  liftIO . putStrLn $ "Environment cleared."
evalString args (stripPrefix ":let " -> Just newbinding) = do
  let (name, binding) = second (dropWhile (==' ')) $ break (==' ') newbinding
  st <- lift get
  when (isJust . lookup name $ st) $
    liftIO . warn $ text "Shadowing existing binding `" <> bold (text name) <> text "'."
  let parsed = parse binding
      evaled = runEval parsed st
  when (showParse args) (liftIO . putStrLn . colorize . ppShow $ parsed)
  case evaled of
    Success s' -> case s' of
      Just y -> do
        lift $ modify ((name, y) :)
        x <- lift get
        liftIO . print $ x
      Nothing -> liftIO . err $ text "Could not produce a valid result."
    Failure d -> liftIO . putStrLn $ show d
evalString _ (stripPrefix ":parse " -> Just expr) =
  case parse expr of
    Success s' -> liftIO . putStrLn . colorize . ppShow $ s'
    Failure d -> liftIO . putStrLn $ show d
evalString args s = liftIO . evalString' args s =<< lift get

-- | Evaluate a String of dash code with some extra "stuff" in the environment.
evalString' :: Arguments -> String -> [(String, Literal)] -> IO ()
evalString' args s st = do
  let parsed = parse s
      evaled = runEval parsed st
  when (showParse args) (liftIO . putStrLn . colorize . ppShow $ parsed)
  case evaled of
   Success s' -> case s' of
     Just y -> liftIO . putStrLn . colorize $ show y
     Nothing -> liftIO . err $ text "Could not produce a valid result."
   Failure d -> liftIO . putStrLn . show $ d

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
parse = parseString (runParser expression) mempty
