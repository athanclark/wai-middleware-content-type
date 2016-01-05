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
  ( lookupFileExt

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

import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Control.Monad

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

