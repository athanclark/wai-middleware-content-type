{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Data.Aeson                              as A
import           Control.Monad.IO.Class                  (MonadIO)




-- | Uses @Json@ as the key in the map, and @"application/json"@ as the content type.
json :: ( A.ToJSON j
        , MonadIO m
        ) => j -> FileExtListenerT (MiddlewareT m) m ()
json = jsonStatusHeaders status200 [("Content-Type", "application/json")]

jsonWith :: ( A.ToJSON j
            , MonadIO m
            ) => (Response -> Response) -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonWith f = jsonStatusHeadersWith f status200 [("Content-Type", "application/json")]

jsonStatus :: ( A.ToJSON j
              , MonadIO m
              ) => Status -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonStatus s = jsonStatusHeaders s [("Content-Type", "application/json")]

jsonStatusWith :: ( A.ToJSON j
                  , MonadIO m
                  ) => (Response -> Response) -> Status -> j
                    -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusWith f s = jsonStatusHeadersWith f s [("Content-Type", "application/json")]

-- | Uses @Json@ as the key in the map, and @"application/javascript"@ as the content type.
jsonp :: ( A.ToJSON j
         , MonadIO m
         ) => j -> FileExtListenerT (MiddlewareT m) m ()
jsonp = jsonStatusHeaders status200 [("Content-Type", "application/javascript")]

jsonpWith :: ( A.ToJSON j
             , MonadIO m
             ) => (Response -> Response) -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonpWith f = jsonStatusHeadersWith f status200 [("Content-Type", "application/javascript")]

jsonpStatus :: ( A.ToJSON j
               , MonadIO m
               ) => Status -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonpStatus s = jsonStatusHeaders s [("Content-Type", "application/javascript")]

jsonpStatusWith :: ( A.ToJSON j
                   , MonadIO m
                   ) => (Response -> Response) -> Status -> j
                     -> FileExtListenerT (MiddlewareT m) m ()
jsonpStatusWith f s = jsonStatusHeadersWith f s [("Content-Type", "application/javascript")]

jsonHeaders :: ( A.ToJSON j
               , MonadIO m
               ) => RequestHeaders -> j
                 -> FileExtListenerT (MiddlewareT m) m ()
jsonHeaders = jsonStatusHeaders status200

jsonHeadersWith :: ( A.ToJSON j
                   , MonadIO m
                   ) => (Response -> Response) -> RequestHeaders -> j
                     -> FileExtListenerT (MiddlewareT m) m ()
jsonHeadersWith f = jsonStatusHeadersWith f status200

jsonStatusHeaders :: ( A.ToJSON j
                     , MonadIO m
                     ) => Status -> RequestHeaders -> j
                       -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusHeaders = jsonStatusHeadersWith id

jsonStatusHeadersWith :: ( A.ToJSON j
                         , MonadIO m
                         ) => (Response -> Response) -> Status -> RequestHeaders -> j
                           -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusHeadersWith f s hs i =
  bytestringStatusWith f Json s hs $ A.encode i




jsonOnly :: A.ToJSON j =>
            j -> Response
jsonOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/json")]

jsonOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/json")]

jsonpOnly :: A.ToJSON j =>
            j -> Response
jsonpOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

jsonpOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonpOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/javascript")]

jsonOnlyHeaders :: A.ToJSON j =>
            RequestHeaders -> j -> Response
jsonOnlyHeaders = jsonOnlyStatusHeaders status200

jsonOnlyStatusHeaders :: A.ToJSON j =>
            Status -> RequestHeaders -> j -> Response
jsonOnlyStatusHeaders s hs i =
  bytestringOnlyStatus s hs $ A.encode i
