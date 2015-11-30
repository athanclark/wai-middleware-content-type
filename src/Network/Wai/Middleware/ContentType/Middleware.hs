module Network.Wai.Middleware.ContentType.Middleware where

import Network.Wai.Middleware.ContentType.Types
import Network.Wai.Trans
import qualified Data.Map as Map


-- | Lifts a @MiddlewareT@ directly as a response to a file extension.
middleware :: Monad m =>
              FileExt
           -> MiddlewareT m
           -> FileExtListenerT (MiddlewareT m) m ()
middleware f m = tell $ Map.singleton f m
