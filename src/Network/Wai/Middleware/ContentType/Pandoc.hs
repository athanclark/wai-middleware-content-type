{-# LANGUAGE
    OverloadedStrings
  #-}


module Network.Wai.Middleware.ContentType.Pandoc where

import           Network.Wai.Middleware.ContentType.Types
import           Network.Wai.Middleware.ContentType.Text
import           Network.HTTP.Types                       (status200, Status, ResponseHeaders)
import           Network.Wai                              (Response)

import qualified Data.Text.Lazy                           as LT
import qualified Text.Pandoc                              as P
import qualified Data.HashMap.Lazy                        as HM


-- * Lifted Combinators

markdown :: Monad m =>
            P.Pandoc
         -> FileExtListenerT m ()
markdown i =
  tell' $ HM.singleton Markdown $
    ResponseVia
      i
      status200
      [("Content-Type","text/markdown")]
      markdownOnly

{-# INLINEABLE markdown #-}


-- * Data Only

markdownOnly :: P.Pandoc -> Status -> ResponseHeaders -> Response
markdownOnly p = case P.runPure (P.writeMarkdown P.def p) of
  Left e -> error (show e)
  Right x -> textOnly (LT.fromStrict x)
