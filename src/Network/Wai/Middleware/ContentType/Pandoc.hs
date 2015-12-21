module Network.Wai.Middleware.ContentType.Pandoc where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Text
import           Network.Wai                              (Response)
import           Network.Wai.HTTP2                        (Body)

import qualified Data.Text.Lazy                           as LT
import qualified Text.Pandoc                              as P
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

markdownResponse :: Monad m => P.Pandoc -> FileExtListenerT Response m ()
markdownResponse i =
  tell' $ HM.singleton Markdown (markdownOnlyResponse i)

{-# INLINEABLE markdownResponse #-}

markdownBody :: Monad m => P.Pandoc -> FileExtListenerT Body m ()
markdownBody i =
  tell' $ HM.singleton Markdown (markdownOnlyBody i)

{-# INLINEABLE markdownBody #-}


-- * Data Only

markdownOnlyResponse :: P.Pandoc -> Response
markdownOnlyResponse =
  textOnlyResponse . LT.pack . P.writeMarkdown P.def

markdownOnlyBody :: P.Pandoc -> Body
markdownOnlyBody =
  textOnlyBody . LT.pack . P.writeMarkdown P.def

