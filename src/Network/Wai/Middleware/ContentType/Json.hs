{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.ByteString
import           Network.HTTP.Types                      (Status, ResponseHeaders)
import           Network.Wai                             (Response)

import qualified Data.Aeson                              as A
import           Control.Monad.IO.Class (MonadIO (..))



-- * Lifted Combinators

json :: ( A.ToJSON j
        , MonadIO m
        ) => j
          -> FileExtListenerT m ()
json =
  (overFileExts [Json] $ mapHeaders (("Content-Type","application/json"):))
  . bytestring Json . A.encode

{-# INLINEABLE json #-}

-- * Data Only

jsonOnly :: A.ToJSON j => j -> Status -> ResponseHeaders -> Response
jsonOnly j =
  bytestringOnly (A.encode j)

{-# INLINEABLE jsonOnly #-}
