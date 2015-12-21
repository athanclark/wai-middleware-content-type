module Network.Wai.Middleware.ContentType.Lucid where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (status200)
import           Network.Wai                             (Response, responseBuilder)
import           Network.Wai.HTTP2                       (Body, streamBuilder)

import qualified Lucid.Base                              as L
import           Control.Monad.Trans
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

lucidResponse :: Monad m =>
                 L.HtmlT m ()
              -> FileExtListenerT Response m ()
lucidResponse i = do
  i' <- lift $ lucidOnlyResponse i
  tell' $ HM.singleton Html i'

{-# INLINEABLE lucidResponse #-}

lucidBody :: Monad m =>
             L.HtmlT m ()
          -> FileExtListenerT Body m ()
lucidBody i = do
  i' <- lift $ lucidOnlyBody i
  tell' $ HM.singleton Html i'

{-# INLINEABLE lucidBody #-}



-- * Data Only

lucidOnlyResponse :: Monad m => L.HtmlT m () -> m Response
lucidOnlyResponse i =
  responseBuilder status200 [] <$> L.execHtmlT i

{-# INLINEABLE lucidOnlyResponse #-}

lucidOnlyBody :: Monad m => L.HtmlT m () -> m Body
lucidOnlyBody i =
  streamBuilder <$> L.execHtmlT i

{-# INLINEABLE lucidOnlyBody #-}
