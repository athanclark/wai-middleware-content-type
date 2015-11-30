{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Clay where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import           Clay.Render
import           Clay.Stylesheet
import qualified Data.Text.Lazy.Encoding                 as LT
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @Text@ as the key in the map, and @"text/css"@ as the content type.
clay :: MonadIO m =>
        Config -> [App] -> Css
     -> FileExtListenerT (MiddlewareT m) m ()
clay c as = clayStatusHeaders c as status200 [("Content-Type", "text/css")]

clayWith :: MonadIO m =>
            (Response -> Response) -> Config -> [App] -> Css
         -> FileExtListenerT (MiddlewareT m) m ()
clayWith f c as = clayStatusHeadersWith f c as status200 [("Content-Type", "text/css")]

clayStatus :: MonadIO m =>
              Config -> [App] -> Status -> Css
           -> FileExtListenerT (MiddlewareT m) m ()
clayStatus c as s = clayStatusHeaders c as s [("Content-Type", "text/css")]

clayStatusWith :: MonadIO m =>
                  (Response -> Response) -> Config -> [App] -> Status -> Css
               -> FileExtListenerT (MiddlewareT m) m ()
clayStatusWith f c as s = clayStatusHeadersWith f c as s [("Content-Type", "text/css")]

clayHeaders :: MonadIO m =>
               Config -> [App] -> RequestHeaders -> Css
            -> FileExtListenerT (MiddlewareT m) m ()
clayHeaders c as = clayStatusHeaders c as status200

clayHeadersWith :: MonadIO m =>
                   (Response -> Response) -> Config -> [App] -> RequestHeaders -> Css
                -> FileExtListenerT (MiddlewareT m) m ()
clayHeadersWith f c as = clayStatusHeadersWith f c as status200

clayStatusHeaders :: MonadIO m =>
                     Config -> [App] -> Status -> RequestHeaders -> Css
                  -> FileExtListenerT (MiddlewareT m) m ()
clayStatusHeaders = clayStatusHeadersWith id

clayStatusHeadersWith :: MonadIO m =>
                         (Response -> Response) -> Config -> [App] -> Status -> RequestHeaders -> Css
                      -> FileExtListenerT (MiddlewareT m) m ()
clayStatusHeadersWith f c as s hs i =
  bytestringStatusWith f Css s hs $ LT.encodeUtf8 $ renderWith c as i



-- * 'Network.Wai.Response' Only

clayOnly :: Config -> [App] -> Css -> Response
clayOnly c as = clayOnlyStatusHeaders c as status200 [("Content-Type", "text/css")]

clayOnlyStatus :: Config -> [App] -> Status -> Css -> Response
clayOnlyStatus c as s = clayOnlyStatusHeaders c as s [("Content-Type", "text/css")]

clayOnlyHeaders :: Config -> [App] -> RequestHeaders -> Css -> Response
clayOnlyHeaders c as = clayOnlyStatusHeaders c as status200

clayOnlyStatusHeaders :: Config -> [App] -> Status -> RequestHeaders -> Css -> Response
clayOnlyStatusHeaders c as s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderWith c as i
