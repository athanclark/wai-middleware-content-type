module Network.Wai.Middleware.ContentType.Middleware where

import Network.Wai.Middleware.ContentType.Types
import Network.Wai.Trans
import qualified Data.Map as Map
import Control.Monad.Writer


-- | Lifts a @MiddlewareT@ directly as a response to a file extension.
middleware :: Monad m =>
              FileExt
           -> MiddlewareT m
           -> FileExtListenerT (MiddlewareT m) m ()
middleware f m = tell $ FileExts $ Map.singleton f m
