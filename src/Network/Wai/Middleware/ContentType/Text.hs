module Network.Wai.Middleware.ContentType.Text where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                       (status200)
import           Network.Wai                              (Response, responseBuilder)
import           Network.Wai.HTTP2                        (Body, streamBuilder)

import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

textResponse :: Monad m => LT.Text -> FileExtListenerT Response m ()
textResponse i =
  tell' $ HM.singleton Text (textOnlyResponse i)

{-# INLINEABLE textResponse #-}

textBody :: Monad m => LT.Text -> FileExtListenerT Body m ()
textBody i =
  tell' $ HM.singleton Text (textOnlyBody i)

{-# INLINEABLE textBody #-}


-- * Data Only

textOnlyResponse :: LT.Text -> Response
textOnlyResponse =
  responseBuilder status200 [] . LT.encodeUtf8Builder

textOnlyBody :: LT.Text -> Body
textOnlyBody =
  streamBuilder . LT.encodeUtf8Builder

