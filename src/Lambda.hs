{-# LANGUAGE OverloadedStrings #-}

module Lambda where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.Maybe
import Data.Text
import Network.AWS.Lambda as Lambda

data VersionData = VersionData
  { version :: Text
  , description :: Text
  } deriving (Show)

makeRevisionData :: FunctionConfiguration -> Maybe VersionData
makeRevisionData fc = liftA2 VersionData (fc ^. fcVersion) (fc ^. fcDescription)

getFunctionNames :: [FunctionConfiguration] -> [Text]
getFunctionNames = mapMaybe $ \f -> f ^. fcFunctionName

listLambdas :: IO [Text]
listLambdas =
  runWithinAWS $ do
    response <- send listFunctions
    return $ getFunctionNames $ response ^. lfrsFunctions

getProperVersions :: ListVersionsByFunctionResponse -> [VersionData]
getProperVersions response =
  Prelude.filter (\(VersionData v _) -> v /= "$LATEST") $
  Prelude.reverse $ mapMaybe makeRevisionData (response ^. lvbfrsVersions)

listLambdaVersions :: Text -> IO [VersionData]
listLambdaVersions fn = runWithinAWS $ invokeRequest fn [] Nothing
  where
    invokeRequest fn acc nextMarker = do
      response <- send $ listVersionsByFunction fn & lvbfMarker .~ nextMarker
      let newNextMarker = response ^. lvbfrsNextMarker
      let revisions = getProperVersions response <> acc
      maybe (return revisions) (invokeRequest fn acc . Just) newNextMarker

publishLambdaVersion :: Text -> Maybe Text -> Text -> IO ()
publishLambdaVersion fn revision desc =
  runWithinAWS $
  void $
  send $
  Lambda.publishVersion fn & pvDescription ?~ desc & pvRevisionId .~ revision

runWithinAWS ::
     (MonadCatch m, MonadUnliftIO m) => AWST' Env (ResourceT m) b -> m b
runWithinAWS f = do
  env <- newEnv Discover
  runResourceT . runAWST env . within Ireland $ f
