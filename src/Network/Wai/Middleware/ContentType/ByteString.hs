module Network.Wai.Middleware.ContentType.ByteString where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (Status, ResponseHeaders)
import           Network.Wai                             (Response, responseLBS)

import qualified Data.ByteString.Lazy                    as LBS
import qualified Blaze.ByteString.Builder.ByteString     as BU
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

bytestring :: Monad m =>
              FileExt
           -> LBS.ByteString
           -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
bytestring fe i =
  tell' $ HM.singleton fe (bytestringOnly i)

{-# INLINEABLE bytestring #-}


-- * Data Only

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnly :: LBS.ByteString -> Status -> ResponseHeaders -> Response
bytestringOnly b s hs = responseLBS s hs b
