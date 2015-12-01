{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Cassius where

import           Network.Wai.Middleware.ContentType.Types as FE
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import           Text.Cassius
import qualified Data.Text.Lazy.Encoding                 as LT
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @cassius@ as the key in the map, and @"cassius/plain"@ as the content type.
cassius :: MonadIO m => Css -> FileExtListenerT (MiddlewareT m) m ()
cassius = cassiusStatusHeaders status200 [("Content-Type", "text/css")]

cassiusWith :: MonadIO m =>
               (Response -> Response) -> Css
            -> FileExtListenerT (MiddlewareT m) m ()
cassiusWith f = cassiusStatusHeadersWith f status200 [("Content-Type", "text/css")]

cassiusStatus :: MonadIO m =>
                 Status -> Css
              -> FileExtListenerT (MiddlewareT m) m ()
cassiusStatus s = cassiusStatusHeaders s [("Content-Type", "text/css")]

cassiusStatusWith :: MonadIO m =>
                     (Response -> Response) -> Status -> Css
                  -> FileExtListenerT (MiddlewareT m) m ()
cassiusStatusWith f s = cassiusStatusHeadersWith f s [("Content-Type", "text/css")]

cassiusHeaders :: MonadIO m =>
                  RequestHeaders -> Css
               -> FileExtListenerT (MiddlewareT m) m ()
cassiusHeaders = cassiusStatusHeaders status200

cassiusHeadersWith :: MonadIO m =>
                      (Response -> Response) -> RequestHeaders -> Css
                   -> FileExtListenerT (MiddlewareT m) m ()
cassiusHeadersWith f = cassiusStatusHeadersWith f status200

cassiusStatusHeaders :: MonadIO m =>
                        Status -> RequestHeaders -> Css
                     -> FileExtListenerT (MiddlewareT m) m ()
cassiusStatusHeaders = cassiusStatusHeadersWith id

cassiusStatusHeadersWith :: MonadIO m =>
                            (Response -> Response) -> Status -> RequestHeaders -> Css
                         -> FileExtListenerT (MiddlewareT m) m ()
cassiusStatusHeadersWith f s hs i =
  bytestringStatusWith f Css s hs $ LT.encodeUtf8 $ renderCss i


-- * 'Network.Wai.Response' Only

cassiusOnly :: Css -> Response
cassiusOnly = cassiusOnlyStatusHeaders status200 [("Content-Type", "text/css")]

cassiusOnlyStatus :: Status -> Css -> Response
cassiusOnlyStatus s = cassiusOnlyStatusHeaders s [("Content-Type", "text/css")]

cassiusOnlyHeaders :: RequestHeaders -> Css -> Response
cassiusOnlyHeaders = cassiusOnlyStatusHeaders status200

cassiusOnlyStatusHeaders :: Status -> RequestHeaders -> Css -> Response
cassiusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderCss i
