module Http where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Default.Class
import Data.Text (Text)
import Network.HTTP.Req
import System.IO
import System.IO.Temp

getCodeContents :: (MonadIO m) => (Url s, Option s) -> m LB.ByteString
getCodeContents (url, options) =
  runReq def $ responseBody <$> req GET url NoReqBody lbsResponse options

saveToTemporaryLocation :: LB.ByteString -> IO FilePath
saveToTemporaryLocation content = do
  (filepath, handle) <- openTempFile "/tmp" "hambdoku.zip"
  LB.hPut handle content
  hClose handle
  return filepath

saveCodeToTemporaryLocation :: ByteString -> IO (Maybe FilePath)
saveCodeToTemporaryLocation codeLocation =
  mapM (saveToTemporaryLocation <=< getCodeContents) parsedUrl
  where
    parsedUrl = parseUrlHttps codeLocation
