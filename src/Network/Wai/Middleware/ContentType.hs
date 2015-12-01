{-# LANGUAGE
    OverloadedStrings
  #-}

{- |
Module      : Network.Wai.Middleware.ContentType
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

Import this module to use all the application end-point combinators:

> {-# LANGUAGE
>     OverloadedStrings
>   #-}
>
> import Network.Wai.Middleware.ContentType
> import qualified Data.Text as T
> import qualified Lucid as L
>
> myApp :: MonadIO m => MiddlewareT
> myApp = fileExtsToMiddleware $ do
>   text "Text!"
>   json ("Json!" :: T.Text)
>   lucid (L.toHtmlRaw ("Html!" :: T.Text))

If you would like to embed a 'Network.Wai.Trans.MiddlewareT' as a response to
a particular supported file extension / content type, import
"Network.Wai.Middleware.ContentType.Middleware":

> import Network.Wai.Middleware.ContentType
> import Network.Wai.Middleware.ContentType.Middleware
>
> myApp = fileExtsToMiddleware $
>   middleware Css myMiddleware
-}

module Network.Wai.Middleware.ContentType
  ( -- * Utilities
    fileExtsToMiddleware
  , lookupResponse
  , possibleFileExts
  , invalidEncoding
  , AcceptHeader
  , -- * Re-Exports
    module Network.Wai.Middleware.ContentType.Types
  , module Network.Wai.Middleware.ContentType.Blaze
  , module Network.Wai.Middleware.ContentType.Builder
  , module Network.Wai.Middleware.ContentType.ByteString
  , module Network.Wai.Middleware.ContentType.Cassius
  , module Network.Wai.Middleware.ContentType.Clay
  , module Network.Wai.Middleware.ContentType.Json
  , module Network.Wai.Middleware.ContentType.Julius
  , module Network.Wai.Middleware.ContentType.Lucid
  , module Network.Wai.Middleware.ContentType.Lucius
  , module Network.Wai.Middleware.ContentType.Text
  , module Network.Wai.Middleware.ContentType.Pandoc
  ) where

import Network.Wai.Trans
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Media (mapAccept)
import Network.Wai.Middleware.ContentType.Types hiding (tell')
import Network.Wai.Middleware.ContentType.Blaze
import Network.Wai.Middleware.ContentType.Builder
import Network.Wai.Middleware.ContentType.ByteString
import Network.Wai.Middleware.ContentType.Cassius
import Network.Wai.Middleware.ContentType.Clay
import Network.Wai.Middleware.ContentType.Json
import Network.Wai.Middleware.ContentType.Julius
import Network.Wai.Middleware.ContentType.Lucid
import Network.Wai.Middleware.ContentType.Lucius
import Network.Wai.Middleware.ContentType.Text
import Network.Wai.Middleware.ContentType.Pandoc

import Network.Wai.Middleware.ContentType.Middleware (middleware)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid
import Control.Monad.Trans
import Control.Monad


type AcceptHeader = BS.ByteString

-- | Turn a map of content types to middlewares, into a middleware.
fileExtsToMiddleware :: ( MonadIO m
                        ) => FileExtListenerT (MiddlewareT m) m ()
                          -> MiddlewareT m
fileExtsToMiddleware contentRoutes app req respond = do
  let mAcceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      mFe       = getFileExt req
  mMiddleware <- lookupResponse mAcceptBS mFe contentRoutes
  fromMaybe (app req respond) $ do
    mid <- mMiddleware
    return $ mid app req respond

-- | Given an HTTP @Accept@ header and a content type to base lookups off of, and
-- a map of responses, find a response.
lookupResponse :: ( MonadIO m
                  ) => Maybe AcceptHeader
                    -> Maybe FileExt
                    -> FileExtListenerT (MiddlewareT m) m ()
                    -> m (Maybe (MiddlewareT m))
lookupResponse mAcceptBS mFe fexts = do
  femap <- execFileExtListenerT fexts
  return $ lookupFileExt femap
  where
    lookupFileExt xs =
      let attempts = findFE $ maybe allFileExts possibleFileExts mAcceptBS
      in  getFirst $ foldMap (First . flip Map.lookup xs) attempts

    findFE :: [FileExt] -> [FileExt]
    findFE xs =
      case mFe of
        Nothing -> []
        Just fe | fe == None -> xs
                | otherwise  -> fe <$ guard (fe `elem` xs)


-- | Takes an @Accept@ header and returns the other
-- file types handleable, in order of prescedence.
possibleFileExts :: AcceptHeader -> [FileExt]
possibleFileExts accept = if not (null wildcard) then wildcard else computed
  where
    computed :: [FileExt]
    computed = concat $
      catMaybes [ mapAccept [ ("application/json"       :: BS.ByteString, [Json])
                            , ("application/javascript" :: BS.ByteString, [JavaScript,Json])
                            ] accept
                , mapAccept [ ("text/html" :: BS.ByteString, [Html])
                            ] accept
                , mapAccept [ ("text/plain" :: BS.ByteString, [Text, Markdown])
                            ] accept
                , mapAccept [ ("text/markdown" :: BS.ByteString, [Markdown])
                            ] accept
                , mapAccept [ ("text/css" :: BS.ByteString, [Css])
                            ] accept
                ]

    wildcard :: [FileExt]
    wildcard = fromMaybe [] $ mapAccept [ ("*/*" :: BS.ByteString, allFileExts)
                                        ] accept



-- | Use this combinator as the last one, as a "catch-all":
--
--   > myApp = do
--   >   text "foo"
--   >   invalidEncoding myErrorHandler
invalidEncoding :: MonadIO m => MiddlewareT m -> FileExtListenerT (MiddlewareT m) m ()
invalidEncoding mid = mapM_ (`middleware` mid) allFileExts


