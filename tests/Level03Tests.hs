{-# LANGUAGE OverloadedStrings #-}
module Level03Tests
  ( unitTests
  ) where

import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.HUnit   (testCase)

import           Network.HTTP.Types as HTTP

import           Helpers            (assertBody, assertStatus, get, post, runTestsFor)

import qualified Level03.Core       as Core

unitTests :: IO ()
unitTests = runTestsFor Core.app "Level 03 Tests" $ do
  -- Using the functions from ``Helpers`` this actions a GET request on the
  -- "/list" route and compares the response body and status code to our
  -- expectations
  get "GET list route" "/list" >>= \resp -> do
    assertBody "List Request not implemented" resp
    assertStatus HTTP.status200 resp

  post "POST add route" "/topic1/add" "comment1" >>= \resp -> do
    assertBody "Hello there!" resp
    assertStatus HTTP.status200 resp

  post "POST add route with empty comment" "/topic1/add" "" >>= \resp -> do
    assertBody "Empty Comment" resp
    assertStatus HTTP.status400 resp

  get "Get view route" "/topic1/view" >>= \resp -> do
    assertBody "View Request not implemented" resp
    assertStatus HTTP.status200 resp


  get "Get view route with empty topic" "//view" >>= \resp -> do
    assertBody "Empty Topic" resp
    assertStatus HTTP.status400 resp

  get "Get undefined route" "/undefined/route" >>= \resp -> do
    assertBody "Unknown Route" resp
    assertStatus HTTP.status404 resp

  -- Write some more tests, below are some ideas to get you started:

  -- Don't worry if you don't get all of these done. :)

  -- 1) The '<topic>/add' route will respond with an error when given an empty comment
  -- 2) The '<topic>/view' route will respond correctly when given a topic
  -- 3) The '<topic>/view' route will respond with an error when given an empty topic
  -- 4) A gibberish route will return a 404

  -- After you're done here, you'll need to uncomment the use of these functions
  -- in the `test/Test.hs` otherwise the tests won't run!
