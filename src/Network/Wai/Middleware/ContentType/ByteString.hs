module Network.Wai.Middleware.ContentType.ByteString where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200)
import           Network.Wai                             (Response, responseLBS)

import qualified Data.ByteString.Lazy                    as LBS
import qualified Blaze.ByteString.Builder.ByteString     as BU
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

bytestring :: Monad m =>
              FileExt
           -> LBS.ByteString
           -> FileExtListenerT Response m ()
bytestring fe i =
  tell' $ HM.singleton fe (bytestringOnly i)

{-# INLINEABLE bytestring #-}


-- * Data Only

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnly :: LBS.ByteString -> Response
bytestringOnly = responseLBS status200 []
