module Network.Wai.Middleware.ContentType.Text where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                       (Status, ResponseHeaders)
import           Network.Wai                              (Response, responseBuilder)

import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

text :: Monad m =>
        LT.Text
     -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
text i =
  tell' $ HM.singleton Text (textOnly i)

{-# INLINEABLE text #-}


-- * Data Only

textOnly :: LT.Text -> Status -> ResponseHeaders -> Response
textOnly t s hs =
  responseBuilder s hs (LT.encodeUtf8Builder t)

