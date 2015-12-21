module Network.Wai.Middleware.ContentType.Blaze where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200)
import           Network.Wai                             (Response, responseBuilder)
import           Network.Wai.HTTP2                       (Body, streamBuilder)

import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Utf8           as H
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

blazeResponse :: Monad m =>
                 H.Html
              -> FileExtListenerT Response m ()
blazeResponse i =
  tell' $ HM.singleton Html (blazeOnlyResponse i)

{-# INLINEABLE blazeResponse #-}

blazeBody :: Monad m =>
             H.Html
          -> FileExtListenerT Body m ()
blazeBody i =
  tell' $ HM.singleton Html (blazeOnlyBody i)

{-# INLINEABLE blazeBody #-}

-- * Data Only

blazeOnlyResponse :: H.Html -> Response
blazeOnlyResponse = responseBuilder status200 [] . H.renderHtmlBuilder

blazeOnlyBody :: H.Html -> Body
blazeOnlyBody = streamBuilder . H.renderHtmlBuilder
