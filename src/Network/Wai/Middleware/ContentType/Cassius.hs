module Network.Wai.Middleware.ContentType.Cassius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)
import           Network.Wai.HTTP2                        (Body)

import           Text.Cassius
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

cassiusResponse :: Monad m =>
                   Css
                -> FileExtListenerT Response m ()
cassiusResponse i =
  tell' $ HM.singleton CT.Css (cassiusOnlyResponse i)

{-# INLINEABLE cassiusResponse #-}

cassiusBody :: Monad m =>
               Css
            -> FileExtListenerT Body m ()
cassiusBody i =
  tell' $ HM.singleton CT.Css (cassiusOnlyBody i)

{-# INLINEABLE cassiusBody #-}


-- * Data Only

cassiusOnlyResponse :: Css -> Response
cassiusOnlyResponse =
  textOnlyResponse . renderCss

cassiusOnlyBody :: Css -> Body
cassiusOnlyBody =
  textOnlyBody . renderCss
