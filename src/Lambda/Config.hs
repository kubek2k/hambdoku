{-# LANGUAGE OverloadedStrings #-}

module Lambda.Config where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text
import Data.Text.IO
import Lambda
import Network.AWS.Lambda as AWSLambda

data RevisionAndVariables =
  RevisionAndVariables (Maybe Text)
                       (Maybe (HashMap Text Text))

getEnvVariables :: FunctionConfiguration -> Maybe (HashMap Text Text)
getEnvVariables conf = conf ^. fcEnvironment >>= \env -> env ^. envVariables

getConfig :: Text -> IO RevisionAndVariables
getConfig fn =
  runWithinAWS $ do
    response <- send $ getFunctionConfiguration fn
    let revision = response ^. fcRevisionId
    let variables = getEnvVariables response
    return $ RevisionAndVariables revision variables

getConfigVariable :: Text -> Text -> IO (Maybe Text)
getConfigVariable fn k = do
  (RevisionAndVariables _ allVars) <- getConfig fn
  return $ allVars >>= HashMap.lookup k

setConfig :: Text -> Maybe Text -> Maybe (HashMap Text Text) -> IO (Maybe Text)
setConfig fn revision config = do
  let newFunctionConfiguration =
        (updateFunctionConfiguration fn &
         ufcEnvironment ?~ (AWSLambda.environment & eVariables .~ config)) &
        ufcRevisionId .~ revision
  runWithinAWS $ do
    response <- send newFunctionConfiguration
    return $ response ^. fcRevisionId

setConfigVariable :: Text -> Text -> Text -> IO ()
setConfigVariable fn k v = do
  (RevisionAndVariables revision currentConfig) <- getConfig fn
  let updatedConfig = insert k v $ fromMaybe HashMap.empty currentConfig
  updatedRevision <- setConfig fn revision $ Just updatedConfig
  publishLambdaVersion fn updatedRevision $
    "Setting configuration variable " <> k

unsetConfigVariable :: Text -> Text -> IO ()
unsetConfigVariable fn k = do
  (RevisionAndVariables revision currentConfig) <- getConfig fn
  let updatedConfig = delete k <$> currentConfig
  updatedRevision <- setConfig fn revision updatedConfig
  publishLambdaVersion fn updatedRevision $
    "Unsetting configuration variable " <> k
