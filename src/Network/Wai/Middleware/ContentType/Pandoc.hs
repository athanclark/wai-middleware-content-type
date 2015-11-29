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
markdown :: MonadIO m => P.Pandoc -> FileExtListenerT (MiddlewareT m) m ()
markdown = markdownStatusHeaders status200 [("Content-Type", "text/markdown")]

markdownWith :: MonadIO m =>
             (Response -> Response) -> P.Pandoc
          -> FileExtListenerT (MiddlewareT m) m ()
markdownWith f = markdownStatusHeadersWith f status200 [("Content-Type", "text/markdown")]

markdownStatus :: MonadIO m =>
               Status -> P.Pandoc
            -> FileExtListenerT (MiddlewareT m) m ()
markdownStatus s = markdownStatusHeaders s [("Content-Type", "text/markdown")]

markdownStatusWith :: MonadIO m =>
                   (Response -> Response) -> Status -> P.Pandoc
                -> FileExtListenerT (MiddlewareT m) m ()
markdownStatusWith f s = markdownStatusHeadersWith f s [("Content-Type", "text/markdown")]

markdownHeaders :: MonadIO m =>
                RequestHeaders -> P.Pandoc
             -> FileExtListenerT (MiddlewareT m) m ()
markdownHeaders = markdownStatusHeaders status200

markdownHeadersWith :: MonadIO m =>
                    (Response -> Response) -> RequestHeaders -> P.Pandoc
                 -> FileExtListenerT (MiddlewareT m) m ()
markdownHeadersWith f = markdownStatusHeadersWith f status200

markdownStatusHeaders :: MonadIO m =>
                      Status -> RequestHeaders -> P.Pandoc
                   -> FileExtListenerT (MiddlewareT m) m ()
markdownStatusHeaders = markdownStatusHeadersWith id

markdownStatusHeadersWith :: MonadIO m =>
                          (Response -> Response) -> Status -> RequestHeaders -> P.Pandoc
                       -> FileExtListenerT (MiddlewareT m) m ()
markdownStatusHeadersWith f s hs i = do
  bytestringStatusWith f Html s hs . LT.encodeUtf8 . LT.pack $ P.writeMarkdown P.def i




markdownOnly :: P.Pandoc -> Response
markdownOnly = markdownOnlyStatusHeaders status200 [("Content-Type", "text/markdown")]

markdownOnlyStatus :: Status -> P.Pandoc -> Response
markdownOnlyStatus s = markdownOnlyStatusHeaders s [("Content-Type", "text/markdown")]

markdownOnlyHeaders :: RequestHeaders -> P.Pandoc -> Response
markdownOnlyHeaders = markdownOnlyStatusHeaders status200

markdownOnlyStatusHeaders :: Status -> RequestHeaders -> P.Pandoc -> Response
markdownOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs . LT.encodeUtf8 . LT.pack $ P.writeMarkdown P.def i
