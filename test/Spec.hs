module Main where

import Network.Wai.Middleware.ContentTypeSpec

import Test.Tasty
import Test.Tasty.Hspec as TH


main :: IO ()
main = do
  wai <- TH.testSpec "Network.Wai.Middleware.ContentType" mockServer
  defaultMain $ testGroup "Testing..."
    [wai]
