{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Lucius where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import           Text.Lucius
import qualified Data.Text.Lazy.Encoding                 as LT
import           Control.Monad.IO.Class                  (MonadIO)


-- * Lifted Combinators

-- | Uses @lucius@ as the key in the map, and @"lucius/css"@ as the content type.
lucius :: MonadIO m => Css -> FileExtListenerT (MiddlewareT m) m ()
lucius = luciusStatusHeaders status200 [("Content-Type", "lucius/css")]

luciusWith :: MonadIO m =>
              (Response -> Response) -> Css
           -> FileExtListenerT (MiddlewareT m) m ()
luciusWith f = luciusStatusHeadersWith f status200 [("Content-Type", "lucius/css")]

luciusStatus :: MonadIO m =>
                Status -> Css
             -> FileExtListenerT (MiddlewareT m) m ()
luciusStatus s = luciusStatusHeaders s [("Content-Type", "lucius/css")]

luciusStatusWith :: MonadIO m =>
                    (Response -> Response) -> Status -> Css
                 -> FileExtListenerT (MiddlewareT m) m ()
luciusStatusWith f s = luciusStatusHeadersWith f s [("Content-Type", "lucius/css")]

luciusHeaders :: MonadIO m =>
                 RequestHeaders -> Css
              -> FileExtListenerT (MiddlewareT m) m ()
luciusHeaders = luciusStatusHeaders status200

luciusHeadersWith :: MonadIO m =>
                     (Response -> Response) -> RequestHeaders -> Css
                  -> FileExtListenerT (MiddlewareT m) m ()
luciusHeadersWith f = luciusStatusHeadersWith f status200

luciusStatusHeaders :: MonadIO m =>
                       Status -> RequestHeaders -> Css
                    -> FileExtListenerT (MiddlewareT m) m ()
luciusStatusHeaders = luciusStatusHeadersWith id

luciusStatusHeadersWith :: MonadIO m =>
                           (Response -> Response) -> Status -> RequestHeaders -> Css
                        -> FileExtListenerT (MiddlewareT m) m ()
luciusStatusHeadersWith f s hs i =
  bytestringStatusWith f Css s hs $ LT.encodeUtf8 $ renderCss i


-- * 'Network.Wai.Response' Only

luciusOnly :: Css -> Response
luciusOnly = luciusOnlyStatusHeaders status200 [("Content-Type", "lucius/css")]

luciusOnlyStatus :: Status -> Css -> Response
luciusOnlyStatus s = luciusOnlyStatusHeaders s [("Content-Type", "lucius/css")]

luciusOnlyHeaders :: RequestHeaders -> Css -> Response
luciusOnlyHeaders = luciusOnlyStatusHeaders status200

luciusOnlyStatusHeaders :: Status -> RequestHeaders -> Css -> Response
luciusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderCss i
