{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Pandoc where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Text.Pandoc                              as P
import           Control.Monad.Trans



-- | Uses the @Html@ key in the map, and @"text/html"@ as the content type.
lucid :: MonadIO m => P.Pandoc -> FileExtListenerT (MiddlewareT m) m ()
lucid = lucidStatusHeaders status200 [("Content-Type", "text/markdown")]

lucidWith :: MonadIO m =>
             (Response -> Response) -> P.Pandoc
          -> FileExtListenerT (MiddlewareT m) m ()
lucidWith f = lucidStatusHeadersWith f status200 [("Content-Type", "text/markdown")]

lucidStatus :: MonadIO m =>
               Status -> P.Pandoc
            -> FileExtListenerT (MiddlewareT m) m ()
lucidStatus s = lucidStatusHeaders s [("Content-Type", "text/markdown")]

lucidStatusWith :: MonadIO m =>
                   (Response -> Response) -> Status -> P.Pandoc
                -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusWith f s = lucidStatusHeadersWith f s [("Content-Type", "text/markdown")]

lucidHeaders :: MonadIO m =>
                RequestHeaders -> P.Pandoc
             -> FileExtListenerT (MiddlewareT m) m ()
lucidHeaders = lucidStatusHeaders status200

lucidHeadersWith :: MonadIO m =>
                    (Response -> Response) -> RequestHeaders -> P.Pandoc
                 -> FileExtListenerT (MiddlewareT m) m ()
lucidHeadersWith f = lucidStatusHeadersWith f status200

lucidStatusHeaders :: MonadIO m =>
                      Status -> RequestHeaders -> P.Pandoc
                   -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusHeaders = lucidStatusHeadersWith id

lucidStatusHeadersWith :: MonadIO m =>
                          (Response -> Response) -> Status -> RequestHeaders -> P.Pandoc
                       -> FileExtListenerT (MiddlewareT m) m ()
lucidStatusHeadersWith f s hs i = do
  bytestringStatusWith f Html s hs . LT.encodeUtf8 . LT.pack $ P.writeMarkdown P.def i




lucidOnly :: P.Pandoc -> Response
lucidOnly = lucidOnlyStatusHeaders status200 [("Content-Type", "text/markdown")]

lucidOnlyStatus :: Status -> P.Pandoc -> Response
lucidOnlyStatus s = lucidOnlyStatusHeaders s [("Content-Type", "text/markdown")]

lucidOnlyHeaders :: RequestHeaders -> P.Pandoc -> Response
lucidOnlyHeaders = lucidOnlyStatusHeaders status200

lucidOnlyStatusHeaders :: Status -> RequestHeaders -> P.Pandoc -> Response
lucidOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs . LT.encodeUtf8 . LT.pack $ P.writeMarkdown P.def i
