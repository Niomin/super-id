{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types
import Network.Wai (Application)
import qualified Data.ByteString.Lazy as BL
import System.Environment (setEnv)

import App (mkApp)
import Repository (initDB)

testImageSize :: Int
testImageSize = 24

testJpgQuality :: Int
testJpgQuality = 10

-- Test app creation
testApp :: IO Application
testApp = do
  setEnv "POSTGRES_HOST" "localhost"
  setEnv "POSTGRES_PORT" "5432"
  setEnv "POSTGRES_USER" "superid"
  setEnv "POSTGRES_PASSWORD" "superid123"
  setEnv "POSTGRES_DB" "superid"
  conn <- initDB
  mkApp conn testImageSize testJpgQuality

spec :: Spec
spec = with testApp $ do

  describe "HELP /" $ do
    it "returns 200 with text/markdown" $
      request "HELP" "/" [] "" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "text/markdown"] }

  describe "RFC2324 - BREW method" $ do
    it "returns 418 I'm a teapot" $
      request "BREW" "/" [] "" `shouldRespondWith` 418

  describe "RFC2324 - WHEN method" $ do
    it "returns 418 I'm a teapot" $
      request "WHEN" "/" [] "" `shouldRespondWith` 418

  describe "RFC2324 - Coffee headers" $ do
    it "returns 418 for coffee Content-Type" $
      request "GET" "/" [("Content-Type", "message/coffeepot")] "" `shouldRespondWith` 418

    it "returns 418 for Coffee Accept-Additions" $
      request "GET" "/" [("Accept-Additions", "Coffee")] "" `shouldRespondWith` 418

  describe "ACQUIRE /" $ do
    it "returns 400 for invalid Content-Type" $
      request "ACQUIRE" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` 400

    it "returns 200 with PNG for valid Dhall request" $ do
      let dhallBody = "{ appId = \"TEST_APP\", payload = { teamId = +42 } } : { appId : Text, payload : { teamId : Integer } }"
      request "ACQUIRE" "/" [("Content-Type", "application/dhall"), ("Accept", "image/png")] (BL.fromStrict dhallBody)
        `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "image/png"] }

  describe "GET /health" $ do
    it "returns 200 OK" $
      get "/health" `shouldRespondWith` "OK" { matchStatus = 200 }
