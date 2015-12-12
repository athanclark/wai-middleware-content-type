module Network.Wai.Middleware.ContentType.Middleware where

import Network.Wai.Middleware.ContentType.Types
import Network.Wai.Trans
import qualified Data.HashMap.Lazy as HM


-- | Lifts a @MiddlewareT@ directly as a response to a file extension.
middleware :: Monad m =>
              FileExt
           -> MiddlewareT m
           -> FileExtListenerT (MiddlewareT m) m ()
middleware f m = tell' (HM.singleton f m)
