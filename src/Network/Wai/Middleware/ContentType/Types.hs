{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , FlexibleContexts
  , DeriveTraversable
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , UndecidableInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module Network.Wai.Middleware.ContentType.Types
  ( FileExt (..)
  , getFileExt
  , toExt
  , FileExts (..)
  , FileExtListenerT (..)
  , execFileExtListenerT
  , mapFileExts
  , tell
  ) where

import           Network.Wai
import qualified Data.Text              as T
import           Data.Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Foldable
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


tell :: (Monoid w, MonadState w m) => w -> m ()
tell x = do
  xs <- get
  put $ xs <> x

-- | Supported file extensions
data FileExt = Html
             | Css
             | JavaScript
             | Json
             | Text
             | Markdown
  deriving (Show, Eq, Ord)


getFileExt :: Request -> FileExt
getFileExt req = fromMaybe Html $ case pathInfo req of
  [] -> Just Html
  xs -> toExt $ T.pack $ getLastExt $ T.unpack $ last xs
  where
    getLastExt ts = evalState (foldrM go [] ts) False
    go c soFar = do
      sawPeriod <- get
      if sawPeriod
      then return soFar
      else if c == '.'
           then do put True
                   return ('.' : soFar)
           else return (c : soFar)

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

newtype FileExts a = FileExts { unFileExts :: Map FileExt a }
  deriving (Show, Eq, Monoid, Functor, Foldable, Traversable)

newtype FileExtListenerT r m a =
  FileExtListenerT { runFileExtListenerT :: StateT (FileExts r) m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO
             , MonadTrans, MonadReader r', MonadWriter w, MonadState (FileExts r)
             , MonadCont, MonadError e, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger, MFunctor
             )

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (FileExtListenerT r m)

instance MonadTransControl (FileExtListenerT r) where
  type StT (FileExtListenerT r) a = StT (StateT (FileExts r)) a
  liftWith = defaultLiftWith FileExtListenerT runFileExtListenerT
  restoreT = defaultRestoreT FileExtListenerT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (FileExtListenerT r m) where
  type StM (FileExtListenerT r m) a = ComposeSt (FileExtListenerT r) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM


execFileExtListenerT :: Monad m => FileExtListenerT r m a -> m (FileExts r)
execFileExtListenerT xs = execStateT (runFileExtListenerT xs) mempty

mapFileExts :: Monad m =>
               (a -> b)
            -> FileExtListenerT a m () -> FileExtListenerT b m ()
mapFileExts f fl = do
  femap <- lift $ execFileExtListenerT fl
  tell $ f <$> femap


