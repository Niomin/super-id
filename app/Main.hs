{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import qualified Web.Scotty as S
import Network.Wai
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as E
import System.Environment (getEnv, lookupEnv)
import Text.Read (readMaybe)

import Repository
import DhallTypes
import Logic

main :: IO ()
main = do
  -- Initialize database connection
  conn <- initDB
  putStrLn "Database connection established"

  -- Read IMAGE_SIZE from environment (default: 24)
  imageSizeStr <- lookupEnv "IMAGE_SIZE"
  let imageSize = maybe 24 (\s -> maybe 24 id (readMaybe s)) imageSizeStr
  putStrLn $ "Image size set to: " ++ show imageSize ++ "px"

  -- Read JPG_QUALITY from environment (default: 10, range: 1-100)
  jpgQualityStr <- lookupEnv "JPG_QUALITY"
  let jpgQuality = maybe 10 (\s -> maybe 10 (max 1 . min 100) (readMaybe s)) jpgQualityStr
  putStrLn $ "JPG quality set to: " ++ show jpgQuality

  -- Start Scotty server with custom method middleware
  putStrLn "Server starting on port 3000..."
  scotty 3000 $ do
    -- Middleware to handle custom HTTP methods
    middleware $ customMethodMiddleware conn imageSize jpgQuality

    -- Health check endpoint
    get "/health" $ do
      text "OK"

-- Middleware to handle custom HTTP methods (ACQUIRE, VALIDATE, ABDICATE)
customMethodMiddleware :: Connection -> Int -> Int -> Middleware
customMethodMiddleware conn imageSize jpgQuality app req respond = do
  let method = requestMethod req
      path = rawPathInfo req

  case (method, path) of
    ("ACQUIRE", "/") -> handleAcquire conn imageSize jpgQuality req respond
    ("VALIDATE", _) | "/" `BL.isPrefixOf` BL.fromStrict path -> handleValidate conn req respond
    ("ABDICATE", _) | "/" `BL.isPrefixOf` BL.fromStrict path -> handleAbdicate conn req respond
    _ -> app req respond

-- ACQUIRE handler
handleAcquire :: Connection -> Int -> Int -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAcquire conn imageSize jpgQuality req respond = do
  result <- (do
    -- Read request body
    bodyBytes <- strictRequestBody req
    let bodyText = TL.toStrict $ TLE.decodeUtf8 bodyBytes

    -- Check content type
    let contentType = lookup "content-type" (requestHeaders req)
    case contentType of
      Just "application/dhall" -> do
        -- Parse Dhall request
        parseResult <- parseDhallAcquireRequest bodyText
        case parseResult of
          Left err -> return $ Left ("Dhall parse error: " ++ err)
          Right acquireReq -> do
            -- Determine response format from Accept header
            let acceptHeader = lookup "accept" (requestHeaders req)
                format = case acceptHeader of
                  Just "image/png" -> FormatPNG
                  Just "image/jpg" -> FormatJPG
                  Just "image/jpeg" -> FormatJPG
                  Just "image/gif" -> FormatGIF
                  Just "audio/x-wav" -> FormatWAV
                  _ -> FormatPNG  -- Default

            -- Call business logic
            (uuid, responseBytes, respContentType) <- acquireIdentity conn acquireReq format imageSize jpgQuality
            return $ Right (responseBytes, respContentType)
      _ -> return $ Left "Invalid content type. Expected application/dhall"
    ) `E.catch` \(e :: E.SomeException) -> return $ Left (show e)

  case result of
    Left err -> respond $ responseLBS status400 [("Content-Type", "text/plain")] (BL.fromStrict $ TE.encodeUtf8 $ T.pack err)
    Right (bytes, contentType) -> respond $ responseLBS status200 [("Content-Type", TE.encodeUtf8 contentType)] bytes

-- VALIDATE handler
handleValidate :: Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleValidate conn req respond = do
  result <- (do
    -- Extract UUID from path
    let path = TE.decodeUtf8 $ rawPathInfo req
        uuidStr = T.drop 1 path  -- Remove leading "/"

    case UUID.fromString (T.unpack uuidStr) of
      Nothing -> return $ Left "Invalid UUID format"
      Just uuid -> do
        -- Read request body
        bodyBytes <- strictRequestBody req
        let bodyText = TL.toStrict $ TLE.decodeUtf8 bodyBytes

        -- Parse Dhall request
        parseResult <- parseDhallValidateRequest bodyText
        case parseResult of
          Left err -> return $ Left ("Dhall parse error: " ++ err)
          Right validateReq -> do
            -- Call business logic
            isValid <- validateIdentityLogic conn uuid validateReq
            if isValid
              then return $ Right ()
              else return $ Left "Validation failed"
    ) `E.catch` \(e :: E.SomeException) -> return $ Left (show e)

  case result of
    Left _ -> respond $ responseLBS status417 [] ""
    Right () -> respond $ responseLBS status204 [] ""

-- ABDICATE handler
handleAbdicate :: Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAbdicate conn req respond = do
  result <- (do
    -- Extract UUID from path
    let path = TE.decodeUtf8 $ rawPathInfo req
        uuidStr = T.drop 1 path  -- Remove leading "/"

    case UUID.fromString (T.unpack uuidStr) of
      Nothing -> return $ Left (403, "Invalid UUID format")
      Just uuid -> do
        -- Read request body
        bodyBytes <- strictRequestBody req
        let bodyText = TL.toStrict $ TLE.decodeUtf8 bodyBytes

        -- Parse Dhall request
        parseResult <- parseDhallAbdicateRequest bodyText
        case parseResult of
          Left err -> return $ Left (400, "Dhall parse error: " ++ err)
          Right abdicateReq -> do
            -- Call business logic
            deleteResult <- abdicateIdentity conn uuid abdicateReq
            case deleteResult of
              Left "Promise field is false or missing" -> return $ Left (403, "Promise field is false or missing")
              Left "Not found" -> return $ Left (404, "Not found")
              Right () -> return $ Right ()
    ) `E.catch` \(e :: E.SomeException) -> return $ Left (500, show e)

  case result of
    Left (code, msg) -> respond $ responseLBS (mkStatus code "") [] ""
    Right () -> respond $ responseLBS status204 [] ""
