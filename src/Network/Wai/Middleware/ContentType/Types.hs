{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
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
  ) where

import           Network.Wai.Trans
import qualified Data.Text              as T
import           Data.Map
import           Data.Monoid
import           Data.Url
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

import           Test.QuickCheck


-- | Version of 'Control.Monad.Writer.tell' for 'Control.Monad.State.StateT'
tell' :: (Monoid w, MonadState w m) => w -> m ()
tell' x = do
  xs <- get
  put $ xs <> x

-- | Supported file extensions
data FileExt = Html
             | Css
             | JavaScript
             | Json
             | Text
             | Markdown
             | None
  deriving (Show, Eq, Ord)

instance Arbitrary FileExt where
  arbitrary = oneof $ pure <$> allFileExts

-- | All file extensions, in the order of likeliness
allFileExts :: [FileExt]
allFileExts = [Html,Text,Json,JavaScript,Css,Markdown]


-- | Gets the known file extension from a Request's 'Network.Wai.pathInfo'.
getFileExt :: Request -> Maybe FileExt
getFileExt req = case pathInfo req of
  [] -> Just None
  xs -> toExt $ snd $ T.breakOn "." $ last xs

-- | matches a file extension (__including__ it's prefix dot - @.html@ for example)
--   to a known one.
toExt :: T.Text -> Maybe FileExt
toExt x | x == ""              = Just None
        | x `elem` htmls       = Just Html
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

type FileExtMap a = Map FileExt a

-- | The monad for our DSL - when using the combinators, our result will be this
--   type:
--
--   > myListener :: FileExtListenerT (MiddlewareT m) m ()
--   > myListener = do
--   >   text "Text!"
--   >   json ("Json!" :: T.Text)
newtype FileExtListenerT r m a =
  FileExtListenerT { runFileExtListenerT :: StateT (FileExtMap r) m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO
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
