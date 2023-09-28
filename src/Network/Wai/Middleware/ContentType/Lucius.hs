{-# LANGUAGE
    OverloadedStrings
  #-}


module Network.Wai.Middleware.ContentType.Lucius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text  (textOnly)
import           Network.HTTP.Types                       (status200, Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Lucius (Css, renderCss)
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

lucius :: Monad m =>
          Css
       -> FileExtListenerT urlbase m ()
lucius i =
  tell' $ HM.singleton CT.Css $
    ResponseVia
      i
      status200
      [("Content-Type","text/css")]
      luciusOnly

{-# INLINEABLE lucius #-}


-- * Data Only

luciusOnly :: Css -> Status -> ResponseHeaders -> Response
luciusOnly c =
  textOnly (renderCss c)
