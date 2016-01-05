{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveGeneric
  , FlexibleContexts
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , UndecidableInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

{- |
Module      : Network.Wai.Middleware.ContentType.Types
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Network.Wai.Middleware.ContentType.Types
  ( -- * Types
    FileExt (..)
  , allFileExts
  , getFileExt
  , toExt
  , FileExtMap
  , FileExtListenerT (..)
  , execFileExtListenerT
  , -- * Utilities
    tell'
  , AcceptHeader
  , possibleFileExts
  , invalidEncoding
  ) where

import qualified Data.Text              as T
import           Data.HashMap.Lazy hiding (null)
import qualified Data.HashMap.Lazy as HM
import           Data.Monoid
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Url
import           Data.Hashable
import qualified Data.ByteString        as BS
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Trans.Control hiding (embed)
import           Control.Monad.Trans.Resource
import           Control.Monad.State
import           Control.Monad.Writer hiding (tell)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Morph

import           GHC.Generics
import           Network.HTTP.Media (mapAccept)


-- | Version of 'Control.Monad.Writer.tell' for 'Control.Monad.State.StateT'
tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = modify' (<> x)

{-# INLINEABLE tell' #-}

-- | Supported file extensions
data FileExt
  = Html
  | Css
  | JavaScript
  | Json
  | Text
  | Markdown
  deriving (Show, Eq, Ord, Generic)

instance Hashable FileExt

-- | All file extensions, in the order of likeliness
allFileExts :: [FileExt]
allFileExts = [Html,Text,Json,JavaScript,Css,Markdown]

{-# INLINEABLE allFileExts #-}

-- | Gets the known file extension from a Request's 'Network.Wai.pathInfo'.
getFileExt :: [T.Text] -> Maybe FileExt
getFileExt chunks = case chunks of
  [] -> Nothing
  xs -> toExt . snd . T.breakOn "." $ last xs

{-# INLINEABLE getFileExt #-}

-- | matches a file extension (__including__ it's prefix dot - @.html@ for example)
--   to a known one.
toExt :: T.Text -> Maybe FileExt
toExt x | x `elem` htmls       = Just Html
        | x `elem` csss        = Just Css
        | x `elem` javascripts = Just JavaScript
        | x `elem` jsons       = Just Json
        | x `elem` texts       = Just Text
        | x `elem` markdowns   = Just Markdown
        | otherwise            = Nothing
  where
    htmls       = [".htm", ".html"]
    csss        = [".css"]
    javascripts = [".js", ".javascript"]
    jsons       = [".json"]
    texts       = [".txt"]
    markdowns   = [".md", ".markdown"]

{-# INLINEABLE toExt #-}

type FileExtMap a = HashMap FileExt a

-- | The monad for our DSL - when using the combinators, our result will be this
--   type:
--
--   > myListener :: FileExtListenerT (MiddlewareT m) m ()
--   > myListener = do
--   >   text "Text!"
--   >   json ("Json!" :: T.Text)
newtype FileExtListenerT r m a = FileExtListenerT
  { runFileExtListenerT :: StateT (FileExtMap r) m a
  } deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO
             , MonadTrans, MonadReader r', MonadWriter w, MonadState (FileExtMap r)
             , MonadCont, MonadError e, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger, MonadUrl b f, MFunctor
             )

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (FileExtListenerT r m)

instance MonadTransControl (FileExtListenerT r) where
  type StT (FileExtListenerT r) a = StT (StateT (FileExtMap r)) a
  liftWith = defaultLiftWith FileExtListenerT runFileExtListenerT
  restoreT = defaultRestoreT FileExtListenerT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (FileExtListenerT r m) where
  type StM (FileExtListenerT r m) a = ComposeSt (FileExtListenerT r) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM


execFileExtListenerT :: Monad m => FileExtListenerT r m a -> m (FileExtMap r)
execFileExtListenerT xs = execStateT (runFileExtListenerT xs) mempty



-- * Headers

type AcceptHeader = BS.ByteString


{-# INLINEABLE execFileExtListenerT #-}

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
invalidEncoding r = mapM_ (\t -> tell' $ HM.singleton t r) allFileExts

{-# INLINEABLE invalidEncoding #-}
