{-# LANGUAGE
    OverloadedStrings
  #-}


module Network.Wai.Middleware.ContentType.Clay where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (status200, Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Clay.Render
import           Clay.Stylesheet
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

clay :: Monad m =>
        Config -> [App] -> Css
     -> FileExtListenerT m ()
clay c as i =
  tell' $ HM.singleton CT.Css $
    ResponseVia
      (c,as,i)
      status200
      [("Content-Type","text/css")]
      (\(c,as,i) -> clayOnly c as i)

{-# INLINEABLE clay #-}


-- * Data Only

clayOnly :: Config -> [App] -> Css -> Status -> ResponseHeaders -> Response
clayOnly c as css =
  textOnly (renderWith c as css)
