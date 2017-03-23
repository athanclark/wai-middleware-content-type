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
  , ExistentialQuantification
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
  , getFileExt
  , toExt
  , ResponseVia (..)
  , runResponseVia
  , mapStatus
  , mapHeaders
  , FileExtMap
  , FileExtListenerT (..)
  , execFileExtListenerT
  , overFileExts
  , mapFileExtMap
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
import           Data.Maybe (fromMaybe)
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
import           Network.HTTP.Types (Status, ResponseHeaders)
import           Network.Wai.Trans (Response)
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
  | Other T.Text -- ^ excluding prefix period, i.e. `foo`
  deriving (Show, Eq, Ord, Generic)

instance Hashable FileExt


-- | Gets the known file extension from a Request's 'Network.Wai.pathInfo'.
getFileExt :: [T.Text] -> Maybe FileExt
getFileExt chunks = case chunks of
  [] -> Nothing
  xs -> toExt . T.breakOnEnd "." $ last xs

{-# INLINEABLE getFileExt #-}

-- | matches a file extension (__including__ it's prefix dot - @.html@ for example)
--   to a known one.
toExt :: (T.Text, T.Text) -> Maybe FileExt
toExt (y,x)
  |    x == ""
    || T.length y == 0
    || T.last y /= '.'   = Nothing
  | x `elem` htmls       = Just Html
  | x `elem` csss        = Just Css
  | x `elem` javascripts = Just JavaScript
  | x `elem` jsons       = Just Json
  | x `elem` texts       = Just Text
  | x `elem` markdowns   = Just Markdown
  | otherwise            = Just $ Other x
  where
    htmls       = ["htm", "html"]
    csss        = ["css"]
    javascripts = ["js", "javascript"]
    jsons       = ["json"]
    texts       = ["txt"]
    markdowns   = ["md", "markdown"]

{-# INLINEABLE toExt #-}


-- ayy, lamo. Basically.
data ResponseVia = forall a. ResponseVia
  { responseData     :: !a
  , responseStatus   :: {-# UNPACK #-} !Status
  , responseHeaders  :: !ResponseHeaders
  , responseFunction :: !(a -> Status -> ResponseHeaders -> Response)
  }

runResponseVia :: ResponseVia -> Response
runResponseVia (ResponseVia d s hs f) = f d s hs

mapStatus :: (Status -> Status) -> ResponseVia -> ResponseVia
mapStatus f (ResponseVia d s hs f') = ResponseVia d (f s) hs f'

mapHeaders :: (ResponseHeaders -> ResponseHeaders) -> ResponseVia -> ResponseVia
mapHeaders f (ResponseVia d s hs f') = ResponseVia d s (f hs) f'

overFileExts :: Monad m =>
                [FileExt]
             -> (ResponseVia -> ResponseVia)
             -> FileExtListenerT m a
             -> FileExtListenerT m a
overFileExts fs f (FileExtListenerT xs) = do
  i <- get
  let i' = HM.mapWithKey (\k x -> if k `elem` fs then f x else x) i
  (x, o) <- lift (runStateT xs i')
  put o
  pure x

type FileExtMap = HashMap FileExt ResponseVia

-- | The monad for our DSL - when using the combinators, our result will be this
--   type:
--
--   > myListener :: FileExtListenerT (MiddlewareT m) m ()
--   > myListener = do
--   >   text "Text!"
--   >   json ("Json!" :: T.Text)
newtype FileExtListenerT m a = FileExtListenerT
  { runFileExtListenerT :: StateT FileExtMap m a
  } deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO
             , MonadTrans, MonadReader r', MonadWriter w, MonadState FileExtMap
             , MonadCont, MonadError e, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger, MonadUrl b f, MFunctor
             )

instance Monad m => Monoid (FileExtListenerT m ()) where
  mempty = FileExtListenerT $ put mempty
  mappend x y = x >> y

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (FileExtListenerT m)

instance MonadTransControl FileExtListenerT where
  type StT FileExtListenerT a = StT (StateT FileExtMap) a
  liftWith = defaultLiftWith FileExtListenerT runFileExtListenerT
  restoreT = defaultRestoreT FileExtListenerT

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (FileExtListenerT m) where
  type StM (FileExtListenerT m) a = ComposeSt FileExtListenerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

execFileExtListenerT :: Monad m => FileExtListenerT m a -> m FileExtMap
execFileExtListenerT xs = execStateT (runFileExtListenerT xs) mempty

mapFileExtMap :: ( Monad m
                 ) => (FileExtMap -> FileExtMap)
                   -> FileExtListenerT m a
                   -> FileExtListenerT m a
mapFileExtMap f (FileExtListenerT xs) = do
  m      <- get
  (x,m') <- lift (runStateT xs (f m))
  put m'
  return x


-- * Headers

type AcceptHeader = BS.ByteString


{-# INLINEABLE execFileExtListenerT #-}

-- | Takes an @Accept@ header and returns the other
-- file types handleable, in order of prescedence.
possibleFileExts :: [FileExt] -> AcceptHeader -> [FileExt]
possibleFileExts allFileExts accept = if not (null wildcard) then wildcard else computed
  where
    computed :: [FileExt]
    computed = fromMaybe [] $
      mapAccept [ ( "application/json"       :: BS.ByteString
                  , [Json]
                  )
                , ( "application/javascript" :: BS.ByteString
                  , [JavaScript,Json]
                  )
                , ( "text/html" :: BS.ByteString
                  , [Html]
                  )
                , ( "text/css" :: BS.ByteString
                  , [Css]
                  )
                , ( "text/markdown" :: BS.ByteString
                  , [Markdown]
                  )
                , ( "text/plain" :: BS.ByteString
                  , [Text, Markdown]
                  )
                ] accept

    wildcard :: [FileExt]
    wildcard = fromMaybe [] $
      mapAccept [ ("*/*" :: BS.ByteString
                  , allFileExts
                  )
                ] accept

{-# INLINEABLE possibleFileExts #-}

-- | Use this combinator as the last one, as a "catch-all":
--
--   > myApp = do
--   >   text "foo"
--   >   invalidEncoding myErrorHandler -- handles all except text/plain
invalidEncoding :: Monad m => ResponseVia -> FileExtListenerT m ()
invalidEncoding r = mapM_ (\t -> tell' $ HM.singleton t r) [Html,Css,JavaScript,Json,Text,Markdown]

{-# INLINEABLE invalidEncoding #-}
