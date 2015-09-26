{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , StandaloneDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Network.Wai.Middleware.ContentType.Types
  ( FileExt (..)
  , getFileExt
  , toExt
  , FileExts (..)
  , FileExtListenerT (..)
  , execFileExtListenerT
  , tell
  ) where

import           Network.Wai
import qualified Data.Text              as T
import           Data.Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Control.Monad.Trans
import           Control.Monad.State


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
  deriving (Show, Eq, Ord)


getFileExt :: Request -> FileExt
getFileExt req = fromMaybe Html $ case pathInfo req of
  [] -> Just Html
  xs -> toExt $ T.dropWhile (/= '.') $ last xs

toExt :: T.Text -> Maybe FileExt
toExt x | x `elem` htmls       = Just Html
        | x `elem` csss        = Just Css
        | x `elem` javascripts = Just JavaScript
        | x `elem` jsons       = Just Json
        | x `elem` texts       = Just Text
        | otherwise            = Nothing
  where
    htmls       = [".htm", ".html"]
    csss        = [".css"]
    javascripts = [".js", ".javascript"]
    jsons       = [".json"]
    texts       = [".txt"]

newtype FileExts a = FileExts { unFileExts :: Map FileExt a }
  deriving (Show, Eq, Monoid, Functor, Foldable, Traversable)

newtype FileExtListenerT r m a =
  FileExtListenerT { runFileExtListenerT :: StateT (FileExts r) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadState (FileExts r))

execFileExtListenerT :: Monad m => FileExtListenerT r m a -> m (FileExts r)
execFileExtListenerT xs = execStateT (runFileExtListenerT xs) mempty

mapFileExts :: Monad m =>
               (a -> b)
            -> FileExtListenerT a m () -> FileExtListenerT b m ()
mapFileExts f fl = do
  femap <- lift $ execFileExtListenerT fl
  tell $ f <$> femap


