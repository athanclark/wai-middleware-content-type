{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Julius where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import           Text.Julius
import qualified Data.Text.Lazy.Encoding                 as LT
import           Control.Monad.IO.Class                  (MonadIO)



-- | Uses @julius@ as the key in the map, and @"application/javascript"@ as the content type.
julius :: MonadIO m =>
          Javascript -> FileExtListenerT (MiddlewareT m) m ()
julius = juliusStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusWith :: MonadIO m =>
              (Response -> Response) -> Javascript
           -> FileExtListenerT (MiddlewareT m) m ()
juliusWith f = juliusStatusHeadersWith f status200 [("Content-Type", "application/javascript")]

juliusStatus :: MonadIO m =>
                Status -> Javascript
             -> FileExtListenerT (MiddlewareT m) m ()
juliusStatus s = juliusStatusHeaders s [("Content-Type", "application/javascript")]

juliusStatusWith :: MonadIO m =>
                    (Response -> Response) -> Status -> Javascript
                 -> FileExtListenerT (MiddlewareT m) m ()
juliusStatusWith f s = juliusStatusHeadersWith f s [("Content-Type", "application/javascript")]

juliusHeaders :: MonadIO m =>
                 RequestHeaders -> Javascript
              -> FileExtListenerT (MiddlewareT m) m ()
juliusHeaders = juliusStatusHeaders status200

juliusHeadersWith :: MonadIO m =>
                     (Response -> Response) -> RequestHeaders -> Javascript
                  -> FileExtListenerT (MiddlewareT m) m ()
juliusHeadersWith f = juliusStatusHeadersWith f status200

juliusStatusHeaders :: MonadIO m =>
                       Status -> RequestHeaders -> Javascript
                    -> FileExtListenerT (MiddlewareT m) m ()
juliusStatusHeaders = juliusStatusHeadersWith id

juliusStatusHeadersWith :: MonadIO m =>
                           (Response -> Response) -> Status -> RequestHeaders -> Javascript
                        -> FileExtListenerT (MiddlewareT m) m ()
juliusStatusHeadersWith f s hs i =
  bytestringStatusWith f Json s hs $ LT.encodeUtf8 $ renderJavascript i



juliusOnly :: Javascript -> Response
juliusOnly = juliusOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusOnlyStatus :: Status -> Javascript -> Response
juliusOnlyStatus s = juliusOnlyStatusHeaders s [("Content-Type", "application/javascript")]

juliusOnlyHeaders :: RequestHeaders -> Javascript -> Response
juliusOnlyHeaders = juliusOnlyStatusHeaders status200

juliusOnlyStatusHeaders :: Status -> RequestHeaders -> Javascript -> Response
juliusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderJavascript i
