module Network.Wai.Middleware.ContentType.Julius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Julius
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

julius :: Monad m =>
          Javascript
       -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
julius i =
  tell' $ HM.singleton CT.JavaScript (juliusOnly i)

{-# INLINEABLE julius #-}


-- * Data Only

juliusOnly :: Javascript -> Status -> ResponseHeaders -> Response
juliusOnly j =
  textOnly (renderJavascript j)
