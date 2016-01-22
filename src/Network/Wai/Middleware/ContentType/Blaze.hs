module Network.Wai.Middleware.ContentType.Blaze where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (Status, ResponseHeaders)
import           Network.Wai                             (Response, responseBuilder)

import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Utf8           as H
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

blaze :: Monad m =>
         H.Html
      -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
blaze i =
  tell' $ HM.singleton Html (blazeOnly i)

{-# INLINEABLE blaze #-}

-- * Data Only

blazeOnly :: H.Html -> Status -> ResponseHeaders -> Response
blazeOnly h s hs = responseBuilder s hs (H.renderHtmlBuilder h)

