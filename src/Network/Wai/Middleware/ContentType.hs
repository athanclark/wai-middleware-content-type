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

-}

module Network.Wai.Middleware.ContentType
  ( -- * Utilities
    lookupFileExt
  , possibleFileExts
  , invalidEncoding
  , AcceptHeader
  , -- * Re-Exports
    module Network.Wai.Middleware.ContentType.Types
  , module Network.Wai.Middleware.ContentType.Blaze
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

import Network.HTTP.Media (mapAccept)
import Network.Wai.Middleware.ContentType.Types hiding (tell')
import Network.Wai.Middleware.ContentType.Blaze
import Network.Wai.Middleware.ContentType.ByteString
import Network.Wai.Middleware.ContentType.Cassius
import Network.Wai.Middleware.ContentType.Clay
import Network.Wai.Middleware.ContentType.Json
import Network.Wai.Middleware.ContentType.Julius
import Network.Wai.Middleware.ContentType.Lucid
import Network.Wai.Middleware.ContentType.Lucius
import Network.Wai.Middleware.ContentType.Text
import Network.Wai.Middleware.ContentType.Pandoc

import qualified Network.Wai.Middleware.ContentType.Types as CT

import qualified Data.ByteString   as BS
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid
import Control.Monad


type AcceptHeader = BS.ByteString

-- | Given an HTTP @Accept@ header and a content type to base lookups off of, and
-- a map of responses, find a response.
lookupFileExt :: Maybe AcceptHeader
              -> Maybe FileExt
              -> FileExtMap r
              -> Maybe r
lookupFileExt mAcceptBS mFe fexts =
  getFirst . foldMap (First . flip HM.lookup fexts) . findFE $
    maybe allFileExts possibleFileExts mAcceptBS
  where
    findFE :: [FileExt] -> [FileExt]
    findFE xs =
      case mFe of
        Nothing -> xs
        Just fe -> fe <$ guard (fe `elem` xs)


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

{-# INLINEABLE possibleFileExts #-}

-- | Use this combinator as the last one, as a "catch-all":
--
--   > myApp = do
--   >   text "foo"
--   >   invalidEncoding myErrorHandler -- handles all except text/plain
invalidEncoding :: Monad m => r -> FileExtListenerT r m ()
invalidEncoding r = mapM_ (\t -> CT.tell' $ HM.singleton t r) allFileExts

{-# INLINEABLE invalidEncoding #-}
