{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Text where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @Text@ as the key in the map, and @"text/plain"@ as the content type.
text :: MonadIO m => LT.Text -> FileExtListenerT (MiddlewareT m) m ()
text = textStatusHeaders status200 [("Content-Type", "text/plain")]

textWith :: MonadIO m =>
            (Response -> Response) -> LT.Text
         -> FileExtListenerT (MiddlewareT m) m ()
textWith f = textStatusHeadersWith f status200 [("Content-Type", "text/plain")]

textStatus :: MonadIO m =>
              Status -> LT.Text
           -> FileExtListenerT (MiddlewareT m) m ()
textStatus s = textStatusHeaders s [("Content-Type", "text/plain")]

textStatusWith :: MonadIO m =>
                  (Response -> Response) -> Status -> LT.Text
               -> FileExtListenerT (MiddlewareT m) m ()
textStatusWith f s = textStatusHeadersWith f s [("Content-Type", "text/plain")]

textHeaders :: MonadIO m =>
               RequestHeaders -> LT.Text
            -> FileExtListenerT (MiddlewareT m) m ()
textHeaders = textStatusHeaders status200

textHeadersWith :: MonadIO m =>
                   (Response -> Response) -> RequestHeaders -> LT.Text
                -> FileExtListenerT (MiddlewareT m) m ()
textHeadersWith f = textStatusHeadersWith f status200

textStatusHeaders :: MonadIO m =>
                     Status -> RequestHeaders -> LT.Text
                  -> FileExtListenerT (MiddlewareT m) m ()
textStatusHeaders = textStatusHeadersWith id

textStatusHeadersWith :: MonadIO m =>
                         (Response -> Response) -> Status -> RequestHeaders -> LT.Text
                      -> FileExtListenerT (MiddlewareT m) m ()
textStatusHeadersWith f s hs i =
  bytestringStatusWith f Text s hs $ LT.encodeUtf8 i



-- * 'Network.Wai.Response' Only

textOnly :: LT.Text -> Response
textOnly = textOnlyStatusHeaders status200 [("Content-Type", "text/plain")]

textOnlyStatus :: Status -> LT.Text -> Response
textOnlyStatus s = textOnlyStatusHeaders s [("Content-Type", "text/plain")]

textOnlyHeaders :: RequestHeaders -> LT.Text -> Response
textOnlyHeaders = textOnlyStatusHeaders status200

textOnlyStatusHeaders :: Status -> RequestHeaders -> LT.Text -> Response
textOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 i
