{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
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
  , fileExtsToMiddleware

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

import Network.Wai (Response, requestHeaders, pathInfo)
import Network.Wai.Trans (MiddlewareT)
import Network.Wai.Logger (withStdoutLogger)
import qualified Data.HashMap.Lazy as HM
import Data.Monoid (First (..))
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (..))
import Data.Singleton.Class (Extractable (..))



-- | Given an HTTP @Accept@ header and a content type to base lookups off of, and
-- a map of responses, find a response.
lookupFileExt :: Maybe AcceptHeader
              -> Maybe FileExt
              -> FileExtMap
              -> Maybe Response
lookupFileExt mAcceptBS mFe m =
    getFirst
  . foldMap (\fe -> First $ runResponseVia <$> HM.lookup fe m)
  . findFE
  $ maybe (HM.keys m) (possibleFileExts $ HM.keys m) mAcceptBS
  where
    findFE :: [FileExt] -> [FileExt]
    findFE xs =
      case mFe of
        Nothing -> xs
        Just fe | fe `elem` xs -> [fe]
                | otherwise    -> []


fileExtsToMiddleware :: MonadBaseControl IO m stM
                     => Extractable stM
                     => FileExtListenerT m a
                     -> MiddlewareT m
fileExtsToMiddleware xs app req respond =
  liftBaseWith $ \runInBase -> withStdoutLogger $ \aplogger -> fmap runSingleton $ runInBase $ do
    m <- execFileExtListenerT xs (Just (aplogger req))
    let mAcceptHeader = lookup "Accept" (requestHeaders req)
        mFileExt      = getFileExt (pathInfo req)
    case lookupFileExt mAcceptHeader mFileExt m of
      Nothing -> app req respond
      Just r  -> respond r
