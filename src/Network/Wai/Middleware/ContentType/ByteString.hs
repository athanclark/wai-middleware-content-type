module Network.Wai.Middleware.ContentType.ByteString where

import           Network.Wai.Middleware.ContentType.Types (FileExtListenerT, tell', getLogger, FileExt, ResponseVia (..))
import           Network.HTTP.Types                      (status200, Status, ResponseHeaders)
import           Network.Wai                             (Response, responseLBS)

import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.HashMap.Lazy                       as HM
import           Control.Monad.IO.Class (MonadIO (..))


-- * Lifted Combinators

bytestring :: Monad m =>
              FileExt
           -> LBS.ByteString
           -> FileExtListenerT urlbase m ()
bytestring fe i = do
  aplogger <- getLogger
  tell' $ HM.singleton fe $
    ResponseVia
      i
      status200
      []
      bytestringOnly

{-# INLINEABLE bytestring #-}


-- * Data Only

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnly :: LBS.ByteString -> Status -> ResponseHeaders -> Response
bytestringOnly b s hs = responseLBS s hs b
