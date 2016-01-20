module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.Wai                             (Response)

import qualified Data.Aeson                              as A



-- * Lifted Combinators

json :: ( A.ToJSON j
        , Monad m
        ) => j
          -> FileExtListenerT Response m ()
json =
  bytestring Json . A.encode

{-# INLINEABLE json #-}

-- * Data Only

jsonOnly :: A.ToJSON j => j -> Response
jsonOnly =
  bytestringOnly . A.encode

{-# INLINEABLE jsonOnly #-}
