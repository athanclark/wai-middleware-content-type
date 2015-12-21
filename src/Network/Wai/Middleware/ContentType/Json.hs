module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.Wai                             (Response)
import           Network.Wai.HTTP2                       (Body)

import qualified Data.Aeson                              as A



-- * Lifted Combinators

jsonResponse :: ( A.ToJSON j
                , Monad m
                ) => j
                  -> FileExtListenerT Response m ()
jsonResponse =
  bytestringResponse Json . A.encode

{-# INLINEABLE jsonResponse #-}

jsonBody :: ( A.ToJSON j
            , Monad m
            ) => j
              -> FileExtListenerT Body m ()
jsonBody =
  bytestringBody Json . A.encode

{-# INLINEABLE jsonBody #-}


-- * Data Only

jsonOnlyResponse :: A.ToJSON j => j -> Response
jsonOnlyResponse =
  bytestringOnlyResponse . A.encode

{-# INLINEABLE jsonOnlyResponse #-}

jsonOnlyBody :: A.ToJSON j => j -> Body
jsonOnlyBody =
  bytestringOnlyBody . A.encode

{-# INLINEABLE jsonOnlyBody #-}
