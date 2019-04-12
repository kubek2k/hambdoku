{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import CommandLine
import Control.Applicative
import Control.Exception as Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.HashMap.Strict as HashMap
import Data.Maybe as Maybe
import Data.Text
import Data.Text.IO as TextIO
import Network.AWS.Lambda as Lambda
import System.Environment
import System.Exit
import System.IO

getFunctionNames :: [FunctionConfiguration] -> [Text]
getFunctionNames = Maybe.mapMaybe $ \f -> f ^. fcFunctionName

listLambdas :: IO [Text]
listLambdas =
  runWithinAWS $ do
    response <- send listFunctions
    return $ getFunctionNames $ response ^. lfrsFunctions

getEnvVariables :: FunctionConfiguration -> Maybe (HashMap Text Text)
getEnvVariables conf = conf ^. fcEnvironment >>= \env -> env ^. envVariables

data RevisionAndVariables =
  RevisionAndVariables (Maybe Text)
                       (Maybe (HashMap Text Text))

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
         ufcEnvironment ?~ (Lambda.environment & eVariables .~ config)) &
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

publishLambdaVersion :: Text -> Maybe Text -> Text -> IO ()
publishLambdaVersion fn revision desc =
  runWithinAWS $
  void $
  send $
  Lambda.publishVersion fn & pvDescription ?~ desc & pvRevisionId .~ revision

data VersionData = VersionData
  { version :: Text
  , description :: Text
  } deriving (Show)

makeRevisionData :: FunctionConfiguration -> Maybe VersionData
makeRevisionData fc = liftA2 VersionData (fc ^. fcVersion) (fc ^. fcDescription)

getProperVersions :: ListVersionsByFunctionResponse -> [VersionData]
getProperVersions response =
  Prelude.filter (\(VersionData v _) -> v /= "$LATEST") $
  Prelude.reverse $ Maybe.mapMaybe makeRevisionData (response ^. lvbfrsVersions)

listLambdaVersions :: Text -> IO [VersionData]
listLambdaVersions fn = runWithinAWS $ invokeRequest fn [] Nothing
  where
    invokeRequest fn acc nextMarker = do
      response <- send $ listVersionsByFunction fn & lvbfMarker .~ nextMarker
      let newNextMarker = response ^. lvbfrsNextMarker
      let revisions = getProperVersions response <> acc
      maybe (return revisions) (invokeRequest fn acc . Just) newNextMarker

runWithinAWS ::
     (MonadCatch m, MonadUnliftIO m) => AWST' Env (ResourceT m) b -> m b
runWithinAWS f = do
  env <- newEnv Discover
  runResourceT . runAWST env . within Ireland $ f

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

main :: IO ()
main = do
  args <- packedArgs
  catching _ServiceError (dispatch args) handleServiceError
  where
    packedArgs :: IO [Text]
    packedArgs = fmap pack <$> getArgs

data User =
  User String
       String
  deriving (Show)
