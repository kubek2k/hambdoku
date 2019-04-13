{-# LANGUAGE OverloadedStrings #-}

module Lambda.Pipelines where

import Data.Maybe
import Data.Set as Set
import Data.Text
import Lambda
import Lambda.Config

downStreamLambdas :: Text
downStreamLambdas = "DOWNSTREAM_LAMBDAS"

getDownstreamLambdas :: Text -> IO [Text]
getDownstreamLambdas fn = do
  varValue <- getConfigVariable fn downStreamLambdas
  return $ splitOn "," $ fromMaybe "" varValue

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
