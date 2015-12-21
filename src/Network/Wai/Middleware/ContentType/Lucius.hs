module Network.Wai.Middleware.ContentType.Lucius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)
import           Network.Wai.HTTP2                        (Body)

import           Text.Lucius
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

luciusResponse :: Monad m => Css -> FileExtListenerT Response m ()
luciusResponse i =
  tell' $ HM.singleton CT.Css (luciusOnlyResponse i)

{-# INLINEABLE luciusResponse #-}

luciusBody :: Monad m => Css -> FileExtListenerT Body m ()
luciusBody i =
  tell' $ HM.singleton CT.Css (luciusOnlyBody i)

{-# INLINEABLE luciusBody #-}


-- * Data Only

luciusOnlyResponse :: Css -> Response
luciusOnlyResponse =
  textOnlyResponse . renderCss

luciusOnlyBody :: Css -> Body
luciusOnlyBody =
  textOnlyBody . renderCss


