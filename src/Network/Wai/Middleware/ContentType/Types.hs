{-# LANGUAGE
    Rank2Types
  , TypeFamilies
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
  , getLogger
  , -- * Utilities
    tell'
  , AcceptHeader
  , possibleFileExts
  , invalidEncoding
  ) where

import qualified Data.Text              as T
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Semigroup (Semigroup)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.Url (MonadUrl)
import           Data.Hashable (Hashable)
import qualified Data.ByteString        as BS
import           Data.Functor.Compose (Compose)
import           Control.Monad (MonadPlus)
import           Control.Applicative (Alternative)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import           Control.Monad.Cont (MonadCont)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Trans.Control (MonadTransControl (..), MonadBaseControl (..), ComposeSt, defaultRestoreM, defaultLiftBaseWith)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.State (StateT (..), MonadState, get, put, execStateT, modify')
import           Control.Monad.Writer (MonadWriter)
import           Control.Monad.Reader (ReaderT (..), MonadReader (..))
import           Control.Monad.Logger (MonadLogger (..))

import           GHC.Generics (Generic)
import           Network.HTTP.Types (Status, ResponseHeaders)
import           Network.HTTP.Media (mapAccept)
import           Network.Wai (Response)


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
  | Other {-# UNPACK #-} !T.Text -- ^ excluding prefix period, i.e. `foo`
  deriving (Show, Eq, Ord, Generic)

instance Hashable FileExt


-- | Gets the known file extension from a Request's 'Network.Wai.pathInfo'.
getFileExt :: [T.Text] -> Maybe FileExt
getFileExt chunks = case chunks of
  [] -> Nothing
  xs -> toExt (T.breakOnEnd "." (last xs))

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
  | otherwise            = Just (Other x)
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
             -> FileExtListenerT urlbase m a
             -> FileExtListenerT urlbase m a
overFileExts fs f (FileExtListenerT (ReaderT xs)) = do
  aplogger <- getLogger
  i <- get
  let i' = HM.mapWithKey (\k x -> if k `elem` fs then f x else x) i
  (x, o) <- lift (runStateT (xs aplogger) i')
  put o
  pure x

type FileExtMap = HashMap FileExt ResponseVia

-- | The monad for our DSL - when using the combinators, our result will be this
--   type:
--
--   > myListener :: FileExtListenerT base (MiddlewareT m) m ()
--   > myListener = do
--   >   text "Text!"
--   >   json ("Json!" :: T.Text)
newtype FileExtListenerT urlbase m a = FileExtListenerT
  { runFileExtListenerT :: ReaderT (Status -> Maybe Integer -> IO ()) (StateT FileExtMap m) a
  } deriving ( Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO
             , MonadWriter w, MonadState FileExtMap
             , MonadCont, MonadError e, MonadBase b, MonadThrow, MonadCatch
             , MonadMask, MonadLogger, MonadUrl urlbase
             )

getLogger :: Monad m => FileExtListenerT urlbase m (Status -> Maybe Integer -> IO ())
getLogger = FileExtListenerT (ReaderT pure)

instance Aligned.MonadTransControl (FileExtListenerT urlbase) ((,) FileExtMap) where
  liftWith client = FileExtListenerT $ ReaderT $ \env -> StateT $ \s ->
    let run :: forall urlbase m a. Monad m => FileExtListenerT urlbase m a -> m (FileExtMap, a)
        run (FileExtListenerT (ReaderT f)) =
          let (StateT g) = f env
          in  do (x, s') <- g s
                 pure (s', x)
    in  do x <- client run
           pure (x, s)
  restoreT mx = FileExtListenerT $ ReaderT $ \_ -> StateT $ \_ -> do
    (s',x) <- mx
    pure (x,s')

instance ( Aligned.MonadBaseControl b m stM
         ) => Aligned.MonadBaseControl b
                (FileExtListenerT urlbase m) (Compose stM ((,) FileExtMap)) where
  liftBaseWith = Aligned.defaultLiftBaseWith
  restoreM = Aligned.defaultRestoreM

instance MonadTrans (FileExtListenerT urlbase) where
  lift m = FileExtListenerT (ReaderT (\_ -> lift m))

instance MonadReader r m => MonadReader r (FileExtListenerT urlbase m) where
  ask = FileExtListenerT (ReaderT (const ask))
  local f (FileExtListenerT (ReaderT g)) = FileExtListenerT $ ReaderT $ \x -> local f (g x)

instance Monad m => Semigroup (FileExtListenerT urlbase m ()) where
  x <> y = x >> y

instance Monad m => Monoid (FileExtListenerT urlbase m ()) where
  mempty = FileExtListenerT (put mempty)

deriving instance (MonadResource m, MonadBase IO m) => MonadResource (FileExtListenerT urlbase m)

instance MonadTransControl (FileExtListenerT urlbase) where
  type StT (FileExtListenerT urlbase) a = StT (StateT FileExtMap)
        (StT (ReaderT (Status -> Maybe Integer -> IO ())) a)
  liftWith f = FileExtListenerT $ ReaderT $ \aplogger -> liftWith $ \runInBase ->
    f (\(FileExtListenerT (ReaderT xs)) -> runInBase (xs aplogger))
  restoreT x = FileExtListenerT $ ReaderT $ \_ -> restoreT x

instance ( MonadBaseControl b m
         ) => MonadBaseControl b (FileExtListenerT urlbase m) where
  type StM (FileExtListenerT urlbase m) a = ComposeSt (FileExtListenerT urlbase) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

execFileExtListenerT :: Monad m
                     => FileExtListenerT urlbase m a
                     -> Maybe (Status -> Maybe Integer -> IO ())
                     -> m FileExtMap
execFileExtListenerT xs mL =
  execStateT
    ( runReaderT (runFileExtListenerT xs)
      (fromMaybe (\_ _ -> pure ()) mL)
    ) mempty

mapFileExtMap :: ( Monad m
                 ) => (FileExtMap -> FileExtMap)
                   -> FileExtListenerT urlbase m a
                   -> FileExtListenerT urlbase m a
mapFileExtMap f (FileExtListenerT xs) = do
  aplogger <- getLogger
  m      <- get
  (x,m') <- lift (runStateT (runReaderT xs aplogger) (f m))
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
invalidEncoding :: Monad m => ResponseVia -> FileExtListenerT urlbase m ()
invalidEncoding r = mapM_ (\t -> tell' (HM.singleton t r)) [Html,Css,JavaScript,Json,Text,Markdown]

{-# INLINEABLE invalidEncoding #-}
