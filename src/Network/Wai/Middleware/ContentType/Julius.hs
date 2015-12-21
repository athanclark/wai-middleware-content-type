module Network.Wai.Middleware.ContentType.Julius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)
import           Network.Wai.HTTP2                        (Body)

import           Text.Julius
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

juliusResponse :: Monad m =>
                  Javascript
               -> FileExtListenerT Response m ()
juliusResponse i =
  tell' $ HM.singleton CT.JavaScript (juliusOnlyResponse i)

{-# INLINEABLE juliusResponse #-}

juliusBody :: Monad m =>
              Javascript
           -> FileExtListenerT Body m ()
juliusBody i =
  tell' $ HM.singleton CT.JavaScript (juliusOnlyBody i)

{-# INLINEABLE juliusBody #-}


-- * Data Only

juliusOnlyResponse :: Javascript -> Response
juliusOnlyResponse =
  textOnlyResponse . renderJavascript

juliusOnlyBody :: Javascript -> Body
juliusOnlyBody =
  textOnlyBody . renderJavascript
