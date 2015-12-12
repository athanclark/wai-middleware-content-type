{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Blaze where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Builder

import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai.Trans
import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Utf8           as H
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @Html@ as the key in the map, and @"text/html"@ as the content type.
blaze :: MonadIO m =>
         H.Html -> FileExtListenerT (MiddlewareT m) m ()
blaze = blazeStatusHeaders status200 [("Content-Type", "text/html")]

{-# INLINEABLE blaze #-}

blazeWith :: MonadIO m =>
             (Response -> Response) -> H.Html
          -> FileExtListenerT (MiddlewareT m) m ()
blazeWith f = blazeStatusHeadersWith f status200 [("Content-Type", "text/html")]

{-# INLINEABLE blazeWith #-}

blazeStatus :: MonadIO m =>
               Status -> H.Html
            -> FileExtListenerT (MiddlewareT m) m ()
blazeStatus s = blazeStatusHeaders s [("Content-Type", "text/html")]

{-# INLINEABLE blazeStatus #-}

blazeStatusWith :: MonadIO m =>
                   (Response -> Response) -> Status -> H.Html
                -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusWith f s = blazeStatusHeadersWith f s [("Content-Type", "text/html")]

{-# INLINEABLE blazeStatusWith #-}

blazeHeaders :: MonadIO m =>
                RequestHeaders -> H.Html
             -> FileExtListenerT (MiddlewareT m) m ()
blazeHeaders = blazeStatusHeaders status200

{-# INLINEABLE blazeHeaders #-}

blazeHeadersWith :: MonadIO m =>
                    (Response -> Response) -> RequestHeaders -> H.Html
                 -> FileExtListenerT (MiddlewareT m) m ()
blazeHeadersWith f = blazeStatusHeadersWith f status200

{-# INLINEABLE blazeHeadersWith #-}

blazeStatusHeaders :: MonadIO m =>
                      Status -> RequestHeaders -> H.Html
                   -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusHeaders = blazeStatusHeadersWith id

{-# INLINEABLE blazeStatusHeaders #-}

blazeStatusHeadersWith :: MonadIO m =>
                          (Response -> Response) -> Status -> RequestHeaders -> H.Html
                       -> FileExtListenerT (MiddlewareT m) m ()
blazeStatusHeadersWith f s hs =
  builderStatusWith f Html s hs . H.renderHtmlBuilder

{-# INLINEABLE blazeStatusHeadersWith #-}

-- * 'Network.Wai.Response' Only

blazeOnly :: H.Html -> Response
blazeOnly = blazeOnlyStatusHeaders status200 [("Content-Type", "text/html")]

{-# INLINEABLE blazeOnly #-}

blazeOnlyHeaders :: RequestHeaders -> H.Html -> Response
blazeOnlyHeaders = blazeOnlyStatusHeaders status200

{-# INLINEABLE blazeOnlyHeaders #-}

blazeOnlyStatus :: Status -> H.Html -> Response
blazeOnlyStatus s = blazeOnlyStatusHeaders s [("Content-Type", "text/html")]

{-# INLINEABLE blazeOnlyStatus #-}

blazeOnlyStatusHeaders :: Status -> RequestHeaders -> H.Html -> Response
blazeOnlyStatusHeaders s hs =
  builderOnlyStatus s hs . H.renderHtmlBuilder

{-# INLINEABLE blazeOnlyStatusHeaders #-}
