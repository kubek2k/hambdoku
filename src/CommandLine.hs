module CommandLine
  ( interpreter
  ) where

import Data.Map as Map
import Data.Maybe (fromMaybe)

errorAction :: String -> ([String] -> IO ())
errorAction subcommand _ =
  putStrLn $ "No matching action was provided for" ++ subcommand

interpreter :: Map String ([String] -> IO ()) -> String -> ([String] -> IO ())
interpreter map command =
  fromMaybe (errorAction command) $ Map.lookup command map
