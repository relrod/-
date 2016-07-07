import Control.Monad.IO.Class
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
    Just s -> liftIO (parseAndDisplayType s) >> loop

main :: IO ()
main = runInputT defaultSettings loop
