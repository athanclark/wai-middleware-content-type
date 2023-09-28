{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType.Json where

import           Network.Wai.Middleware.ContentType.Types (FileExtListenerT, FileExt (Json), mapHeaders, overFileExts)
import           Network.Wai.Middleware.ContentType.ByteString (bytestringOnly, bytestring)
import           Network.HTTP.Types                      (Status, ResponseHeaders)
import           Network.Wai                             (Response)

import qualified Data.Aeson                              as A
import           Control.Monad.IO.Class (MonadIO (..))



-- * Lifted Combinators

json :: ( A.ToJSON j
        , Monad m
        ) => j
          -> FileExtListenerT urlbase m ()
json =
  overFileExts [Json] (mapHeaders (("Content-Type","application/json"):))
    . bytestring Json . A.encode

{-# INLINEABLE json #-}

-- * Data Only

jsonOnly :: A.ToJSON j => j -> Status -> ResponseHeaders -> Response
jsonOnly j =
  bytestringOnly (A.encode j)

{-# INLINEABLE jsonOnly #-}
