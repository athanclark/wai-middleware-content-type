{-# LANGUAGE
    OverloadedStrings
  #-}


module Network.Wai.Middleware.ContentType.Cassius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text  (textOnly)
import           Network.HTTP.Types                       (status200, Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Cassius (Css, renderCss)
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

cassius :: Monad m =>
           Css
        -> FileExtListenerT m ()
cassius i =
  tell' $ HM.singleton CT.Css $
    ResponseVia
      i
      status200
      [("Content-Type", "text/css")]
      cassiusOnly

{-# INLINEABLE cassius #-}


-- * Data Only

cassiusOnly :: Css -> Status -> ResponseHeaders -> Response
cassiusOnly c =
  textOnly (renderCss c)
