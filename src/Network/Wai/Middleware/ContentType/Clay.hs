module Network.Wai.Middleware.ContentType.Clay where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)

import           Clay.Render
import           Clay.Stylesheet
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

clay :: Monad m =>
        Config -> [App] -> Css
     -> FileExtListenerT Response m ()
clay c as i =
  tell' $ HM.singleton CT.Css (clayOnly c as i)

{-# INLINEABLE clay #-}


-- * Data Only

clayOnly :: Config -> [App] -> Css -> Response
clayOnly c as =
  textOnly . renderWith c as
