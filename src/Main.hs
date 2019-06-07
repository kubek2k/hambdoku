{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS
import Data.HashMap.Strict as HashMap
import Data.Text
import Data.Text.Encoding
import Data.Text.IO as TextIO
import Http
import Lambda
import Lambda.Config as Config
import Lambda.Pipelines as Pipelines
import System.Environment
import System.Exit
import System.IO

printEnvVars :: HashMap Text Text -> IO ()
printEnvVars m = mapM_ printEnvVar entries
  where
    entries = toList m
    printEnvVar (k, v) = TextIO.putStrLn $ k <> "=" <> v

handleServiceError e = do
  forM_ (e ^. serviceMessage) $ \(ErrorMessage m) ->
    TextIO.putStrLn $ "ERROR: " <> m
  exitFailure

dispatch :: [Text] -> IO ()
dispatch ("list":_) = do
  lambdas <- listLambdas
  mapM_ TextIO.putStrLn lambdas
dispatch ("config":fn:_) = do
  (RevisionAndVariables _ configM) <- getConfig fn
  mapM_ printEnvVars configM
dispatch ("config:get":fn:varName:_) =
  getConfigVariable fn varName >>= mapM_ TextIO.putStrLn
dispatch ("config:set":fn:varName:varValue:_) =
  setConfigVariable fn varName varValue
dispatch ("config:unset":fn:varName:_) = unsetConfigVariable fn varName
dispatch ("releases":fn:_) = do
  versions <- listLambdaVersions fn
  mapM_ (\v -> TextIO.putStrLn (version v <> " " <> description v)) versions
dispatch ("download":fn:_) = downloadLambda fn >>= mapM_ print
dispatch ("pipelines:add":fn:downstream:_) = addDownstreamLambda fn downstream
dispatch ("pipelines:remove":fn:downstream:_) =
  removeDownstreamLambda fn downstream
dispatch ("pipelines:promote":fn:_) = promoteLambda fn
dispatch ("pipelines:info":fn:_) = getDownstreamLambdas fn >>= mapM_ TextIO.putStrLn
dispatch v = System.IO.putStrLn $ "No support for " <> show v

main :: IO ()
main = do
  args <- packedArgs
  catching _ServiceError (dispatch args) handleServiceError
  where
    packedArgs :: IO [Text]
    packedArgs = fmap pack <$> getArgs
