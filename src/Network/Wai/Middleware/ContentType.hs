{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType
  ( module X
  , fileExtsToMiddleware
  , lookupResponse
  , possibleFileExts
  ) where

import Network.Wai.Middleware.ContentType.Types      as X hiding (tell)
import Network.Wai.Middleware.ContentType.Blaze      as X
import Network.Wai.Middleware.ContentType.Builder    as X
import Network.Wai.Middleware.ContentType.ByteString as X
import Network.Wai.Middleware.ContentType.Cassius    as X
import Network.Wai.Middleware.ContentType.Clay       as X
import Network.Wai.Middleware.ContentType.Json       as X
import Network.Wai.Middleware.ContentType.Julius     as X
import Network.Wai.Middleware.ContentType.Lucid      as X
import Network.Wai.Middleware.ContentType.Lucius     as X
import Network.Wai.Middleware.ContentType.Text       as X
import Network.Wai.Middleware.ContentType.Pandoc     as X

import Network.Wai.Trans
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Media (mapAccept)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.List (intersect, nub)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid
import Control.Monad.Trans


type AcceptHeader = BS.ByteString

-- | Turn a map of content types to middlewares, into a middleware.
fileExtsToMiddleware :: MonadIO m =>
                        FileExtListenerT (MiddlewareT m) m ()
                     -> MiddlewareT m
fileExtsToMiddleware contentRoutes app req respond = do
  let mAcceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      mFe = getFileExt req
  mMiddleware <- lookupResponse mAcceptBS mFe contentRoutes
  fromMaybe (app req respond) $ do
    mid <- mMiddleware
    return $ mid app req respond

-- | Given an HTTP @Accept@ header and a content type to base lookups off of, and
-- a map of responses, find a response.
lookupResponse :: Monad m =>
                  Maybe AcceptHeader
               -> Maybe FileExt
               -> FileExtListenerT (MiddlewareT m) m ()
               -> m (Maybe (MiddlewareT m))
lookupResponse mAcceptBS mFe fexts = do
  femap <- execFileExtListenerT fexts
  return $ lookupFileExt femap
  where
    lookupFileExt xs =
      let attempts = maybe allFileExts (possibleFileExts mFe) mAcceptBS
      in  getFirst $ foldMap (\f' -> First $ Map.lookup f' xs) attempts


-- | Takes a file extension and an @Accept@ header, and returns the other
-- file types handleable, in order of prescedence.
possibleFileExts :: Maybe FileExt -> AcceptHeader -> [FileExt]
possibleFileExts mFe accept =
  let computed :: [FileExt]
      computed = sortFE $ nub $ concat $
        catMaybes [ mapAccept [ ("application/json"       :: BS.ByteString, [Json])
                              , ("application/javascript" :: BS.ByteString, [Json,JavaScript])
                              ] accept
                  , mapAccept [ ("text/html" :: BS.ByteString, [Html])
                              ] accept
                  , mapAccept [ ("text/plain" :: BS.ByteString, [Text])
                              ] accept
                  , mapAccept [ ("text/markdown" :: BS.ByteString, [Markdown, Text])
                              ] accept
                  , mapAccept [ ("text/css" :: BS.ByteString, [Css])
                              ] accept
                  ]

      wildcard :: [FileExt]
      wildcard = sortFE $ concat $
        catMaybes [ mapAccept [ ("*/*" :: BS.ByteString, allFileExts)
                              ] accept
                  ]
  in if not (null wildcard) then wildcard else computed
  where
    sortFE :: [FileExt] -> [FileExt]
    sortFE xs = maybe xs (\fe -> go fe `intersect` xs) mFe
      where
        go Html       = htmls
        go JavaScript = javascripts
        go Json       = jsons
        go Css        = csss
        go Text       = texts
        go Markdown   = markdowns

        htmls       = [Html, Text]
        javascripts = [JavaScript, Text]
        jsons       = [Json, JavaScript, Text]
        csss        = [Css, Text]
        texts       = [Text]
        markdowns   = [Markdown, Text]


-- | All file extensions, in the order of preference
allFileExts :: [FileExt]
allFileExts = [Html,Text,Json,JavaScript,Css,Markdown]

