{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (RequestHeaders, Status, status200)
import           Network.Wai.Trans

import qualified Data.Aeson                              as A
import           Control.Monad.IO.Class                  (MonadIO)



-- * Lifted Combinators

-- | Uses @Json@ as the key in the map, and @"application/json"@ as the content type.
json :: ( A.ToJSON j
        , MonadIO m
        ) => j -> FileExtListenerT (MiddlewareT m) m ()
json = jsonStatusHeaders status200 [("Content-Type", "application/json")]

{-# INLINEABLE json #-}

jsonWith :: ( A.ToJSON j
            , MonadIO m
            ) => (Response -> Response) -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonWith f = jsonStatusHeadersWith f status200 [("Content-Type", "application/json")]

{-# INLINEABLE jsonWith #-}

jsonStatus :: ( A.ToJSON j
              , MonadIO m
              ) => Status -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonStatus s = jsonStatusHeaders s [("Content-Type", "application/json")]

{-# INLINEABLE jsonStatus #-}

jsonStatusWith :: ( A.ToJSON j
                  , MonadIO m
                  ) => (Response -> Response) -> Status -> j
                    -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusWith f s = jsonStatusHeadersWith f s [("Content-Type", "application/json")]

{-# INLINEABLE jsonStatusWith #-}

-- | Uses @Json@ as the key in the map, and @"application/javascript"@ as the content type.
jsonp :: ( A.ToJSON j
         , MonadIO m
         ) => j -> FileExtListenerT (MiddlewareT m) m ()
jsonp = jsonStatusHeaders status200 [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonp #-}

jsonpWith :: ( A.ToJSON j
             , MonadIO m
             ) => (Response -> Response) -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonpWith f = jsonStatusHeadersWith f status200 [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonpWith #-}

jsonpStatus :: ( A.ToJSON j
               , MonadIO m
               ) => Status -> j -> FileExtListenerT (MiddlewareT m) m ()
jsonpStatus s = jsonStatusHeaders s [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonpStatus #-}

jsonpStatusWith :: ( A.ToJSON j
                   , MonadIO m
                   ) => (Response -> Response) -> Status -> j
                     -> FileExtListenerT (MiddlewareT m) m ()
jsonpStatusWith f s = jsonStatusHeadersWith f s [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonpStatusWith #-}

jsonHeaders :: ( A.ToJSON j
               , MonadIO m
               ) => RequestHeaders -> j
                 -> FileExtListenerT (MiddlewareT m) m ()
jsonHeaders = jsonStatusHeaders status200

{-# INLINEABLE jsonHeaders #-}

jsonHeadersWith :: ( A.ToJSON j
                   , MonadIO m
                   ) => (Response -> Response) -> RequestHeaders -> j
                     -> FileExtListenerT (MiddlewareT m) m ()
jsonHeadersWith f = jsonStatusHeadersWith f status200

{-# INLINEABLE jsonHeadersWith #-}

jsonStatusHeaders :: ( A.ToJSON j
                     , MonadIO m
                     ) => Status -> RequestHeaders -> j
                       -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusHeaders = jsonStatusHeadersWith id

{-# INLINEABLE jsonStatusHeaders #-}

jsonStatusHeadersWith :: ( A.ToJSON j
                         , MonadIO m
                         ) => (Response -> Response) -> Status -> RequestHeaders -> j
                           -> FileExtListenerT (MiddlewareT m) m ()
jsonStatusHeadersWith f s hs =
  bytestringStatusWith f Json s hs . A.encode

{-# INLINEABLE jsonStatusHeadersWith #-}


-- * 'Network.Wai.Response' Only

jsonOnly :: A.ToJSON j =>
            j -> Response
jsonOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/json")]

{-# INLINEABLE jsonOnly #-}

jsonOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/json")]

{-# INLINEABLE jsonOnlyStatus #-}

jsonpOnly :: A.ToJSON j =>
            j -> Response
jsonpOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonpOnly #-}

jsonpOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonpOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/javascript")]

{-# INLINEABLE jsonpOnlyStatus #-}

jsonOnlyHeaders :: A.ToJSON j =>
            RequestHeaders -> j -> Response
jsonOnlyHeaders = jsonOnlyStatusHeaders status200

{-# INLINEABLE jsonOnlyHeaders #-}

jsonOnlyStatusHeaders :: A.ToJSON j =>
            Status -> RequestHeaders -> j -> Response
jsonOnlyStatusHeaders s hs =
  bytestringOnlyStatus s hs . A.encode

{-# INLINEABLE jsonOnlyStatusHeaders #-}
