module Network.Wai.Middleware.ContentType.Cassius where

import           Network.Wai.Middleware.ContentType.Types as CT
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)

import           Text.Cassius
import qualified Data.Text.Lazy.Encoding                  as LT
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

cassius :: Monad m =>
           Css
        -> FileExtListenerT Response m ()
cassius i =
  tell' $ HM.singleton CT.Css (cassiusOnly i)

{-# INLINEABLE cassius #-}


-- * Data Only

cassiusOnly :: Css -> Response
cassiusOnly =
  textOnly . renderCss
