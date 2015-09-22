{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Middleware.ContentType
  ( module X
  , fileExtsToMiddleware
  , lookupResponse
  , possibleFileExts
  ) where

import Network.Wai.Middleware.ContentType.Types      as X
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

fileExtsToMiddleware :: MonadIO m =>
                        FileExtListenerT (MiddlewareT m) m ()
                     -> MiddlewareT m
fileExtsToMiddleware contentRoutes app req respond = do
  let mAcceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
  mMiddleware <- lookupResponse mAcceptBS fe contentRoutes
  fromMaybe (app req respond) $ do
    m <- mMiddleware
    return $ m app req respond


lookupResponse :: Monad m =>
                  Maybe AcceptHeader
               -> FileExt
               -> FileExtListenerT a m ()
               -> m (Maybe a)
lookupResponse mAcceptBS f fexts = do
  femap <- execFileExtListenerT fexts
  return $ lookupFileExt mAcceptBS f femap
  where
    lookupFileExt mAccept k (FileExts xs) =
      let attempts = maybe [Html,Text,Json,JavaScript,Css]
                       (possibleFileExts k) mAccept
      in getFirst $ foldMap (\f' -> First $ Map.lookup f' xs) attempts


-- | Takes a file extension and an @Accept@ header, and returns the other
-- file types handleable, in order of prescedence.
possibleFileExts :: FileExt -> AcceptHeader -> [FileExt]
possibleFileExts fe accept =
  let computed = sortFE fe $ nub $ concat $
        catMaybes [ mapAccept [ ("application/json" :: BS.ByteString, [Json])
                              , ("application/javascript" :: BS.ByteString, [Json,JavaScript])
                              ] accept
                  , mapAccept [ ("text/html" :: BS.ByteString, [Html])
                              ] accept
                  , mapAccept [ ("text/plain" :: BS.ByteString, [Text])
                              ] accept
                  , mapAccept [ ("text/css" :: BS.ByteString, [Css])
                              ] accept
                  ]

      wildcard = concat $
        catMaybes [ mapAccept [ ("*/*" :: BS.ByteString, [Html,Text,Json,JavaScript,Css])
                              ] accept
                  ]
  in if not (null wildcard) then wildcard else computed
  where
    sortFE Html       xs = [Html, Text]             `intersect` xs
    sortFE JavaScript xs = [JavaScript, Text]       `intersect` xs
    sortFE Json       xs = [Json, JavaScript, Text] `intersect` xs
    sortFE Css        xs = [Css, Text]              `intersect` xs
    sortFE Text       xs = [Text]                   `intersect` xs



