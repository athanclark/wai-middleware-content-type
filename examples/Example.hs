{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Trans
import Network.Wai.Middleware.ContentType
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Lucid     as L
import Control.Monad.IO.Class


data AppError = AppError

myListener :: MonadIO m => FileExtListenerT (MiddlewareT m) m ()
myListener = do
  text "Text!"
  json ("Json!" :: T.Text)
  lucid (L.toHtmlRaw ("Html!" :: T.Text))

myApp :: MonadIO m => MiddlewareT m
myApp = fileExtsToMiddleware myListener

main :: IO ()
main = run 3000 (myApp defApp)
  where
    defApp :: Application
    defApp _ respond =
      respond $ textOnlyStatus status404 "404"
