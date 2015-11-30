{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Blaze where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString

import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai.Trans
import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Text           as H
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @Html@ as the key in the map, and @"text/html"@ as the content type.
blaze :: MonadIO m =>
         H.Html -> FileExtListenerT (MiddlewareT m) m ()
blaze = blazeStatusHeaders status200 [("Content-Type", "text/html")]

blazeWith :: MonadIO m =>
             (Response -> Response) -> H.Html
          -> FileExtListenerT (MiddlewareT m) m ()
blazeWith f = blazeStatusHeadersWith f status200 [("Content-Type", "text/html")]

blazeStatus :: MonadIO m =>
               Status -> H.Html
            -> FileExtListenerT (MiddlewareT m) m ()
blazeStatus s = blazeStatusHeaders s [("Content-Type", "text/html")]

blazeStatusWith :: MonadIO m =>
                   (Response -> Response) -> Status -> H.Html
                -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusWith f s = blazeStatusHeadersWith f s [("Content-Type", "text/html")]

blazeHeaders :: MonadIO m =>
                RequestHeaders -> H.Html
             -> FileExtListenerT (MiddlewareT m) m ()
blazeHeaders = blazeStatusHeaders status200

blazeHeadersWith :: MonadIO m =>
                    (Response -> Response) -> RequestHeaders -> H.Html
                 -> FileExtListenerT (MiddlewareT m) m ()
blazeHeadersWith f = blazeStatusHeadersWith f status200

blazeStatusHeaders :: MonadIO m =>
                      Status -> RequestHeaders -> H.Html
                   -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusHeaders = blazeStatusHeadersWith id

blazeStatusHeadersWith :: MonadIO m =>
                          (Response -> Response) -> Status -> RequestHeaders -> H.Html
                       -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusHeadersWith f s hs i =
  bytestringStatusWith f Html s hs $ LT.encodeUtf8 $ H.renderHtml i


-- * 'Network.Wai.Response' Only

blazeOnly :: H.Html -> Response
blazeOnly = blazeOnlyStatusHeaders status200 [("Content-Type", "text/html")]

blazeOnlyHeaders :: RequestHeaders -> H.Html -> Response
blazeOnlyHeaders = blazeOnlyStatusHeaders status200

blazeOnlyStatus :: Status -> H.Html -> Response
blazeOnlyStatus s = blazeOnlyStatusHeaders s [("Content-Type", "text/html")]

blazeOnlyStatusHeaders :: Status -> RequestHeaders -> H.Html -> Response
blazeOnlyStatusHeaders s hs i =
  bytestringOnlyStatus s hs $ LT.encodeUtf8 $ H.renderHtml i
