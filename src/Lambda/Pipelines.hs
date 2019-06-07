{-# LANGUAGE OverloadedStrings #-}

module Lambda.Pipelines where

import Control.Monad
import Data.Maybe
import Data.Set as Set
import Data.Text
import Data.Text.IO as TextIO
import Lambda
import Lambda.Config

downStreamLambdas :: Text
downStreamLambdas = "DOWNSTREAM_LAMBDAS"

getDownstreamLambdas :: Text -> IO [Text]
getDownstreamLambdas fn = do
  varValue <- getConfigVariable fn downStreamLambdas
  return $ maybe [] (splitOn ",") $ mfilter (not . Data.Text.null) varValue

getDownstreamLambdasSet :: Text -> IO (Set Text)
getDownstreamLambdasSet fn = fromList <$> getDownstreamLambdas fn

setDownstreamLambdasSet :: Text -> Set Text -> IO ()
setDownstreamLambdasSet fn lambdas =
  let lambdasValue = intercalate "," $ toList lambdas
   in setConfigVariable fn downStreamLambdas lambdasValue

addDownstreamLambda :: Text -> Text -> IO ()
addDownstreamLambda owningFn fn = do
  oldDownstreamLambdas <- getDownstreamLambdasSet owningFn
  let newDownstreamLambdas = insert fn oldDownstreamLambdas
  setDownstreamLambdasSet owningFn newDownstreamLambdas

removeDownstreamLambda :: Text -> Text -> IO ()
removeDownstreamLambda owningFn fn = do
  oldDownstreamLambdas <- getDownstreamLambdasSet owningFn
  let newDownstreamLambdas = delete fn oldDownstreamLambdas
  setDownstreamLambdasSet owningFn newDownstreamLambdas

promoteLambda :: Text -> IO ()
promoteLambda fn = do
  downstreamLambdas <- getDownstreamLambdas fn
  if not (Prelude.null downstreamLambdas)
    then do
      temporaryCodeLocation <- maybeToList <$> downloadLambda fn
      sequence_ $ liftM2 uploadLambda temporaryCodeLocation downstreamLambdas
    else TextIO.putStrLn $
         "No " <> downStreamLambdas <> " variable set or it's empty"
