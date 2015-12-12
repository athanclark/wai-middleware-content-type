{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Clay where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Builder
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

{-# INLINEABLE clay #-}

clayWith :: MonadIO m =>
            (Response -> Response) -> Config -> [App] -> Css
         -> FileExtListenerT (MiddlewareT m) m ()
clayWith f c as = clayStatusHeadersWith f c as status200 [("Content-Type", "text/css")]

{-# INLINEABLE clayWith #-}

clayStatus :: MonadIO m =>
              Config -> [App] -> Status -> Css
           -> FileExtListenerT (MiddlewareT m) m ()
clayStatus c as s = clayStatusHeaders c as s [("Content-Type", "text/css")]

{-# INLINEABLE clayStatus #-}

clayStatusWith :: MonadIO m =>
                  (Response -> Response) -> Config -> [App] -> Status -> Css
               -> FileExtListenerT (MiddlewareT m) m ()
clayStatusWith f c as s = clayStatusHeadersWith f c as s [("Content-Type", "text/css")]

{-# INLINEABLE clayStatusWith #-}

clayHeaders :: MonadIO m =>
               Config -> [App] -> RequestHeaders -> Css
            -> FileExtListenerT (MiddlewareT m) m ()
clayHeaders c as = clayStatusHeaders c as status200

{-# INLINEABLE clayHeaders #-}

clayHeadersWith :: MonadIO m =>
                   (Response -> Response) -> Config -> [App] -> RequestHeaders -> Css
                -> FileExtListenerT (MiddlewareT m) m ()
clayHeadersWith f c as = clayStatusHeadersWith f c as status200

{-# INLINEABLE clayHeadersWith #-}

clayStatusHeaders :: MonadIO m =>
                     Config -> [App] -> Status -> RequestHeaders -> Css
                  -> FileExtListenerT (MiddlewareT m) m ()
clayStatusHeaders = clayStatusHeadersWith id

{-# INLINEABLE clayStatusHeaders #-}

clayStatusHeadersWith :: MonadIO m =>
                         (Response -> Response) -> Config -> [App] -> Status -> RequestHeaders -> Css
                      -> FileExtListenerT (MiddlewareT m) m ()
clayStatusHeadersWith f c as s hs =
  builderStatusWith f Css s hs . LT.encodeUtf8Builder . renderWith c as

{-# INLINEABLE clayStatusHeadersWith #-}


-- * 'Network.Wai.Response' Only

clayOnly :: Config -> [App] -> Css -> Response
clayOnly c as = clayOnlyStatusHeaders c as status200 [("Content-Type", "text/css")]

{-# INLINEABLE clayOnly #-}

clayOnlyStatus :: Config -> [App] -> Status -> Css -> Response
clayOnlyStatus c as s = clayOnlyStatusHeaders c as s [("Content-Type", "text/css")]

{-# INLINEABLE clayOnlyStatus #-}

clayOnlyHeaders :: Config -> [App] -> RequestHeaders -> Css -> Response
clayOnlyHeaders c as = clayOnlyStatusHeaders c as status200

{-# INLINEABLE clayOnlyHeaders #-}

clayOnlyStatusHeaders :: Config -> [App] -> Status -> RequestHeaders -> Css -> Response
clayOnlyStatusHeaders c as s hs = builderOnlyStatus s hs . LT.encodeUtf8Builder . renderWith c as

{-# INLINEABLE clayOnlyStatusHeaders #-}
