module Network.Wai.Middleware.ContentType.Lucid where

import           Network.Wai.Middleware.ContentType.Types
import           Network.HTTP.Types                      (Status, ResponseHeaders)
import           Network.Wai                             (Response, responseBuilder)

import qualified Lucid.Base                              as L
import           Control.Monad.Trans
import qualified Data.HashMap.Lazy                       as HM


-- * Lifted Combinators

lucid :: Monad m =>
         L.HtmlT m ()
      -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
lucid i = do
  i' <- lift (lucidOnly i)
  tell' (HM.singleton Html i')

{-# INLINEABLE lucid #-}


-- * Data Only

lucidOnly :: Monad m => L.HtmlT m () -> m (Status -> ResponseHeaders -> Response)
lucidOnly i =
  (\b s hs -> responseBuilder s hs b) <$> L.execHtmlT i

{-# INLINEABLE lucidOnly #-}
