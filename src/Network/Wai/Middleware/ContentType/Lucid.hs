{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType.Lucid where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200, Status, ResponseHeaders)
import           Network.Wai                             (Response, responseBuilder)

import qualified Lucid.Base                              as L
import           Control.Monad.Trans
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

lucid :: Monad m =>
         L.HtmlT m ()
      -> FileExtListenerT m ()
lucid i = do
  f <- lift (lucidOnly i)
  tell' $ HM.singleton Html $
    ResponseVia
      i
      status200
      [("Content-Type","text/html")]
      (const f)

{-# INLINEABLE lucid #-}


-- * Data Only

lucidOnly :: Monad m => L.HtmlT m () -> m (Status -> ResponseHeaders -> Response)
lucidOnly i =
  (\b s hs -> responseBuilder s hs b) <$> L.execHtmlT i

{-# INLINEABLE lucidOnly #-}
