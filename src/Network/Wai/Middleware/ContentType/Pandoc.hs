module Network.Wai.Middleware.ContentType.Pandoc where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (Status, ResponseHeaders)
import           Network.Wai                              (Response)

import qualified Data.Text.Lazy                           as LT
import qualified Text.Pandoc                              as P
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

markdown :: Monad m =>
            P.Pandoc
         -> FileExtListenerT (Status -> ResponseHeaders -> Response) m ()
markdown i =
  tell' $ HM.singleton Markdown (markdownOnly i)

{-# INLINEABLE markdown #-}


-- * Data Only

markdownOnly :: P.Pandoc -> Status -> ResponseHeaders -> Response
markdownOnly p =
  textOnly (LT.pack $ P.writeMarkdown P.def p)
