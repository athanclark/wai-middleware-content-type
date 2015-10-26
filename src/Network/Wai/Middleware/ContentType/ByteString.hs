{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.ByteString where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Middleware
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans
import qualified Network.Wai.Util                        as U

import qualified Data.ByteString.Lazy                    as B
import           Data.Map
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class


-- * Lifted @MiddlewareT@

-- | @ByteString@ is ambiguous - we need to know what @RequestHeaders@ and @FileExt@ should be associated.
bytestring :: MonadIO m => FileExt -> RequestHeaders -> B.ByteString
           -> FileExtListenerT (MiddlewareT m) m ()
bytestring e = bytestringStatus e status200

bytestringWith :: MonadIO m => (Response -> Response) -> FileExt -> RequestHeaders -> B.ByteString
               -> FileExtListenerT (MiddlewareT m) m ()
bytestringWith f e = bytestringStatusWith f e status200

bytestringStatus :: MonadIO m => FileExt -> Status -> RequestHeaders -> B.ByteString
                 -> FileExtListenerT (MiddlewareT m) m ()
bytestringStatus = bytestringStatusWith id

bytestringStatusWith :: MonadIO m =>
                        (Response -> Response)
                     -> FileExt
                     -> Status
                     -> RequestHeaders
                     -> B.ByteString
                     -> FileExtListenerT (MiddlewareT m) m ()
bytestringStatusWith f fe s hs i = do
  r <- lift $ U.bytestring s hs i
  middleware fe $ \_ _ respond -> respond (f r)


-- * Raw @Response@s

bytestringOnly :: RequestHeaders -> B.ByteString -> Response
bytestringOnly = bytestringOnlyStatus status200

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnlyStatus :: Status -> RequestHeaders -> B.ByteString -> Response
bytestringOnlyStatus = responseLBS
