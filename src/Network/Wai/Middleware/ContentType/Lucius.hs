module Network.Wai.Middleware.ContentType.Lucius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (Status, ResponseHeaders)
import           Network.Wai                              (Response)

import           Text.Lucius
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

lucius :: Monad m =>
          Css
       -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
lucius i =
  tell' $ HM.singleton CT.Css (luciusOnly i)

{-# INLINEABLE lucius #-}


-- * Data Only

luciusOnly :: Css -> Status -> ResponseHeaders -> Response
luciusOnly c =
  textOnly (renderCss c)
