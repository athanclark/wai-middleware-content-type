{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  #-}

module Network.Wai.Middleware.ContentTypeSpec where

import Network.Wai.Middleware.ContentType
  ( FileExtListenerT, FileExt (Other), bytestring, julius, cassius, json, lucid, text
  , textOnly, fileExtsToMiddleware
  )
import Network.Wai (Application)
import Network.HTTP.Types (status406)
import qualified Data.Text      as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Lucid          as L
import qualified Text.Lucius    as SL
import qualified Text.Julius    as SJ
import Test.Hspec (Spec, it, describe)
import Test.Hspec.Wai   as HW



mockServer :: Spec
mockServer = do
  describe "All Requests Respond to the Accept Header" $
    with (return app) $ do
      describe "Text" $
        it "should respond with 200" $
        HW.request "GET" "/" [("Accept", "text/plain")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Text!"
           }
      describe "Json" $
        it "should respond with 200" $
        HW.request "GET" "/" [("Accept", "application/json")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "\"Json!\""
           }
      describe "Html" $
        it "should respond with 200" $
        HW.request "GET" "/" [("Accept", "text/html")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Html!"
           }
      describe "Css" $
        it "should respond with 200" $
        HW.request "GET" "/" [("Accept", "text/css")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "body{background:#fff}"
           }
      describe "JavaScript" $
        it "should respond with 200" $
        HW.request "GET" "/" [("Accept", "application/javascript")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "function foo () {return;}"
           }
  describe "All Requests Respond to the File Extension" $
    with (return app) $ do
      describe "Text" $
        it "should respond with 200" $
        HW.get "/index.txt" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Text!"
           }
      describe "Json" $
        it "should respond with 200" $
        HW.get "/index.json" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "\"Json!\""
           }
      describe "Html" $
        it "should respond with 200" $
        HW.get "index.html" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Html!"
           }
      describe "Css" $
        it "should respond with 200" $
        HW.get "/index.css" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "body{background:#fff}"
           }
      describe "JavaScript" $
        it "should respond with 200" $
        HW.get "/index.js" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "function foo () {return;}"
           }
      describe "Foo" $
        it "should respond with 200" $
        HW.get "/index.foo" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Foo"
           }
  describe "All Requests Respond to Both" $
    with (return app) $ do
      describe "Text" $
        it "should respond with 200" $
        HW.request "GET" "/index.txt" [("Accept", "text/plain")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Text!"
           }
      describe "Json" $
        it "should respond with 200" $
        HW.request "GET" "/index.json" [("Accept", "application/json")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "\"Json!\""
           }
      describe "Html" $
        it "should respond with 200" $
        HW.request "GET" "/index.html" [("Accept", "text/html")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Html!"
           }
      describe "Css" $
        it "should respond with 200" $
        HW.request "GET" "/index.css" [("Accept", "text/css")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "body{background:#fff}"
           }
      describe "JavaScript" $
        it "should respond with 200" $
        HW.request "GET" "/index.js" [("Accept", "application/javascript")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "function foo () {return;}"
           }
  describe "Outlier behavior" $
    with (return app) $ do
      describe "No extension, with header" $
        it " should respond with 200" $
        HW.request "GET" "/index" [("Accept", "text/plain")] "" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = "Text!"
           }
  describe "Unfulfillable accept headers should break" $
    with (return app) $ do
      describe "Text on non-ContentType" $
        it "should respond with 406" $
        HW.request "GET" "/index.txt" [("Accept", "foo/bar")] "" `shouldRespondWith`
        406

app :: Application
app =
  fileExtsToMiddleware allExamples $
  (\req respond -> respond (textOnly "Something went wrong" status406 []))

allExamples :: FileExtListenerT IO ()
allExamples = do
  text "Text!"
  json ("Json!" :: T.Text)
  lucid (L.toHtmlRaw ("Html!" :: T.Text))
  cassius ([SL.lucius|body {background: #fff;}|] undefined)
  julius  ([SJ.julius|function foo () {return;}|] undefined)
  bytestring (Other "foo") (LT.encodeUtf8 "Foo")
