{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Builder where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Data.ByteString.Builder                 as BU
import qualified Data.HashMap.Lazy                       as HM
import           Control.Monad.IO.Class


-- * Lifted Combinators

-- | A builder is ambiguous, therefore we require @RequestHeaders@ and a @FileExt@ to be explicitly
-- supplied.
builder :: MonadIO m =>
           FileExt -> RequestHeaders -> BU.Builder
        -> FileExtListenerT (MiddlewareT m) m ()
builder e  = builderStatus e status200

builderWith :: MonadIO m =>
               (Response -> Response) -> FileExt -> RequestHeaders -> BU.Builder
            -> FileExtListenerT (MiddlewareT m) m ()
builderWith f e = builderStatusWith f e status200

builderStatus :: MonadIO m =>
                 FileExt -> Status -> RequestHeaders -> BU.Builder
              -> FileExtListenerT (MiddlewareT m) m ()
builderStatus = builderStatusWith id

builderStatusWith :: MonadIO m =>
                     (Response -> Response) -> FileExt -> Status -> RequestHeaders -> BU.Builder
                  -> FileExtListenerT (MiddlewareT m) m ()
builderStatusWith f e s hs i =
  let r = builderOnlyStatus s hs i in
  FileExtListenerT $ tell' $
    HM.singleton e $ \_ _ respond -> respond (f r)


-- * 'Network.Wai.Response' Only

builderOnly :: RequestHeaders -> BU.Builder -> Response
builderOnly = builderOnlyStatus status200

-- | The exact same thing as @Network.Wai.responseBuilder@.
builderOnlyStatus :: Status -> RequestHeaders -> BU.Builder -> Response
builderOnlyStatus = responseBuilder
