module Network.Wai.Middleware.ContentType.ByteString where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200)
import           Network.Wai                             (Response, responseLBS)
import           Network.Wai.HTTP2                       (Body, streamBuilder)

import qualified Data.ByteString.Lazy                    as LBS
import qualified Blaze.ByteString.Builder.ByteString     as BU
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

bytestringResponse :: Monad m =>
                      FileExt
                   -> LBS.ByteString
                   -> FileExtListenerT Response m ()
bytestringResponse fe i =
  tell' $ HM.singleton fe (bytestringOnlyResponse i)

{-# INLINEABLE bytestringResponse #-}

bytestringBody :: Monad m =>
                  FileExt
               -> LBS.ByteString
               -> FileExtListenerT Body m ()
bytestringBody fe i =
  tell' $ HM.singleton fe (bytestringOnlyBody i)

{-# INLINEABLE bytestringBody #-}


-- * Data Only

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnlyResponse :: LBS.ByteString -> Response
bytestringOnlyResponse = responseLBS status200 []

bytestringOnlyBody :: LBS.ByteString -> Body
bytestringOnlyBody = streamBuilder . BU.fromLazyByteString
