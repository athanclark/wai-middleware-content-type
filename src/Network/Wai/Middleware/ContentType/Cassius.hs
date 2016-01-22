module Network.Wai.Middleware.ContentType.Cassius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Cassius
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

cassius :: Monad m =>
           Css
        -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
cassius i =
  tell' $ HM.singleton CT.Css (cassiusOnly i)

{-# INLINEABLE cassius #-}


-- * Data Only

cassiusOnly :: Css -> Status -> ResponseHeaders -> Response
cassiusOnly c =
  textOnly (renderCss c)
