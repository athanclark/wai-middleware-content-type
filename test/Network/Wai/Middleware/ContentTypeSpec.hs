module Network.Wai.Middleware.ContentTypeSpec (spec) where

import Network.Wai.Middleware.ContentType

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Network.Wai.Middleware.ContentType"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
