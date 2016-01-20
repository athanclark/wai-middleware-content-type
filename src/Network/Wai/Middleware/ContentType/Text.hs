module Network.Wai.Middleware.ContentType.Text where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                       (status200)
import           Network.Wai                              (Response, responseBuilder)

import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

text :: Monad m => LT.Text -> FileExtListenerT Response m ()
text i =
  tell' $ HM.singleton Text (textOnly i)

{-# INLINEABLE text #-}


-- * Data Only

textOnly :: LT.Text -> Response
textOnly =
  responseBuilder status200 [] . LT.encodeUtf8Builder

