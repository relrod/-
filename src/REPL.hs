import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import System.Console.Haskeline
--import Text.Trifecta

import Language.Dash.Utility
--import Language.Dash.Parser.Skye

parseAndDisplayType :: String -> IO ()
parseAndDisplayType = parsePrint

loop :: InputT IO ()
loop = do
  input <- getInputLine "dash> "
  case input of
    Nothing -> return ()
    Just "quit" -> return ()
    Just s
      | ":ast " `isPrefixOf` s -> liftIO (printAST (drop 5 s)) >> loop
      | otherwise -> liftIO (parseAndDisplayType s) >> loop

main :: IO ()
main = runInputT defaultSettings loop
