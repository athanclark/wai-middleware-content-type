{-# LANGUAGE
    OverloadedStrings
  #-}


module Network.Wai.Middleware.ContentType.Julius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text  (textOnly)
import           Network.HTTP.Types                       (status200, Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Julius (Javascript (..), renderJavascript)
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

julius :: Monad m =>
          Javascript
       -> FileExtListenerT m ()
julius i =
  tell' $ HM.singleton CT.JavaScript $
    ResponseVia
      i
      status200
      [("Content-Type","application/javascript")]
      juliusOnly

{-# INLINEABLE julius #-}


-- * Data Only

juliusOnly :: Javascript -> Status -> ResponseHeaders -> Response
juliusOnly j =
  textOnly (renderJavascript j)
