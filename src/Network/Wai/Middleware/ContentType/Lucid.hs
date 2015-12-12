{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Lucid where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Builder
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Lucid.Base                              as L
import           Control.Monad.Trans


-- * Lifted Combinators

-- | Uses the @Html@ key in the map, and @"text/html"@ as the content type.
lucid :: MonadIO m => L.HtmlT m () -> FileExtListenerT (MiddlewareT m) m ()
lucid = lucidStatusHeaders status200 [("Content-Type", "text/html")]

{-# INLINEABLE lucid #-}

lucidWith :: MonadIO m =>
             (Response -> Response) -> L.HtmlT m ()
          -> FileExtListenerT (MiddlewareT m) m ()
lucidWith f = lucidStatusHeadersWith f status200 [("Content-Type", "text/html")]

{-# INLINEABLE lucidWith #-}

lucidStatus :: MonadIO m =>
               Status -> L.HtmlT m ()
            -> FileExtListenerT (MiddlewareT m) m ()
lucidStatus s = lucidStatusHeaders s [("Content-Type", "text/html")]

{-# INLINEABLE lucidStatus #-}

lucidStatusWith :: MonadIO m =>
                   (Response -> Response) -> Status -> L.HtmlT m ()
                -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusWith f s = lucidStatusHeadersWith f s [("Content-Type", "text/html")]

{-# INLINEABLE lucidStatusWith #-}

lucidHeaders :: MonadIO m =>
                RequestHeaders -> L.HtmlT m ()
             -> FileExtListenerT (MiddlewareT m) m ()
lucidHeaders = lucidStatusHeaders status200

{-# INLINEABLE lucidHeaders #-}

lucidHeadersWith :: MonadIO m =>
                    (Response -> Response) -> RequestHeaders -> L.HtmlT m ()
                 -> FileExtListenerT (MiddlewareT m) m ()
lucidHeadersWith f = lucidStatusHeadersWith f status200

{-# INLINEABLE lucidHeadersWith #-}

lucidStatusHeaders :: MonadIO m =>
                      Status -> RequestHeaders -> L.HtmlT m ()
                   -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusHeaders = lucidStatusHeadersWith id

{-# INLINEABLE lucidStatusHeaders #-}

lucidStatusHeadersWith :: MonadIO m =>
                          (Response -> Response) -> Status -> RequestHeaders -> L.HtmlT m ()
                       -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusHeadersWith f s hs i = do
  i' <- lift $ L.execHtmlT i
  builderStatusWith f Html s hs i'

{-# INLINEABLE lucidStatusHeadersWith #-}



-- * 'Network.Wai.Response' Only

lucidOnly :: Monad m =>
             L.HtmlT m () -> m Response
lucidOnly = lucidOnlyStatusHeaders status200 [("Content-Type", "text/html")]

{-# INLINEABLE lucidOnly #-}

lucidOnlyStatus :: Monad m =>
             Status -> L.HtmlT m () -> m Response
lucidOnlyStatus s = lucidOnlyStatusHeaders s [("Content-Type", "text/html")]

{-# INLINEABLE lucidOnlyStatus #-}

lucidOnlyHeaders :: Monad m =>
             RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyHeaders = lucidOnlyStatusHeaders status200

{-# INLINEABLE lucidOnlyHeaders #-}

lucidOnlyStatusHeaders :: Monad m =>
             Status -> RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyStatusHeaders s hs i = builderOnlyStatus s hs <$> L.execHtmlT i

{-# INLINEABLE lucidOnlyStatusHeaders #-}
