{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType.Blaze where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200, Status, ResponseHeaders)
import           Network.Wai                             (Response, responseBuilder)

import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Utf8           as H
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

blaze :: Monad m =>
         H.Html
      -> FileExtListenerT m ()
blaze i =
  tell' $ HM.singleton Html $
    ResponseVia
      i
      status200
      [("Content-Type", "text/html")]
      blazeOnly

{-# INLINEABLE blaze #-}

-- * Data Only

blazeOnly :: H.Html -> Status -> ResponseHeaders -> Response
blazeOnly h s hs = responseBuilder s hs (H.renderHtmlBuilder h)

