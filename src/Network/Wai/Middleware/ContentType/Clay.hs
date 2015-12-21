module Network.Wai.Middleware.ContentType.Clay where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)
import           Network.Wai.HTTP2                        (Body)

import           Clay.Render
import           Clay.Stylesheet
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

clayResponse :: Monad m =>
                Config -> [App] -> Css
             -> FileExtListenerT Response m ()
clayResponse c as i =
  tell' $ HM.singleton CT.Css (clayOnlyResponse c as i)

{-# INLINEABLE clayResponse #-}

clayBody :: Monad m =>
            Config -> [App] -> Css
         -> FileExtListenerT Body m ()
clayBody c as i =
  tell' $ HM.singleton CT.Css (clayOnlyBody c as i)

{-# INLINEABLE clayBody #-}


-- * Data Only

clayOnlyResponse :: Config -> [App] -> Css -> Response
clayOnlyResponse c as =
  textOnlyResponse . renderWith c as

clayOnlyBody :: Config -> [App] -> Css -> Body
clayOnlyBody c as =
  textOnlyBody . renderWith c as
