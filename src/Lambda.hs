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
import Data.Text.Encoding
import Http
import Network.AWS.Lambda as Lambda

data VersionData = VersionData
  { version :: Text
  , description :: Text
  } deriving (Show)

listLambdas :: IO [Text]
listLambdas =
  runWithinAWS $ do
    response <- send listFunctions
    return $ getFunctionNames $ response ^. lfrsFunctions
  where
    getFunctionNames = mapMaybe $ \f -> f ^. fcFunctionName

getProperVersions :: ListVersionsByFunctionResponse -> [VersionData]
getProperVersions response =
  Prelude.filter (\(VersionData v _) -> v /= "$LATEST") $
  Prelude.reverse $ mapMaybe makeRevisionData (response ^. lvbfrsVersions)
  where
    makeRevisionData fc =
      liftA2 VersionData (fc ^. fcVersion) (fc ^. fcDescription)

listLambdaVersions :: Text -> IO [VersionData]
listLambdaVersions fn = runWithinAWS $ invokeRequest fn [] Nothing
  where
    invokeRequest fn acc nextMarker = do
      response <- send $ listVersionsByFunction fn & lvbfMarker .~ nextMarker
      let newNextMarker = response ^. lvbfrsNextMarker
      let revisions = getProperVersions response <> acc
      maybe (return revisions) (invokeRequest fn acc . Just) newNextMarker

getLambdaCodeLocation :: Text -> IO (Maybe Text)
getLambdaCodeLocation fn =
  runWithinAWS $ do
    response <- send $ getFunction fn
    return $ response ^. gfrsCode & fmap (view fclLocation) & join

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

downloadLambda :: Text -> IO (Maybe FilePath)
downloadLambda fn = do
  codeLocation <- getLambdaCodeLocation fn
  let encodedLocation = encodeUtf8 <$> codeLocation
  fmap join $ forM encodedLocation saveCodeToTemporaryLocation
