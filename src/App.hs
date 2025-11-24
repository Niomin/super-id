{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (mkApp, teapots, getRandomTeapot, isCoffeeRequest, customMethodMiddleware) where

import Web.Scotty
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
import System.Random (randomRIO)
import qualified Data.ByteString as BS

import Repository
import DhallTypes
import Logic

-- Create the WAI application (for testing and production)
mkApp :: Connection -> Int -> Int -> IO Application
mkApp conn imageSize jpgQuality = scottyApp $ do
  -- Middleware to handle custom HTTP methods and RFC2324
  middleware $ customMethodMiddleware conn imageSize jpgQuality

  -- Health check endpoint
  get "/health" $ do
    text "OK"

-- ASCII teapots for RFC2324
teapots :: [String]
teapots =
  [ unlines
      [ "    _...._"
      , "  .'      `."
      , " :  Coffee?  :"
      , " :    No!    :"
      , "  `._______.'"
      , "      _|_|_"
      , "     (_____)  I'm a teapot!"
      ]
  , unlines
      [ "        ___"
      , "       {___}"
      , "     .-'   '-."
      , "    /  Tea    \\"
      , "   |   Only!   |"
      , "    \\  .....  /"
      , "     '-._._.-'"
      , "       |_|_|"
      ]
  , unlines
      [ "      .--."
      , "     |    |"
      , "     |    |  <-- I'm a teapot"
      , "    .'    '."
      , "   /   __   \\"
      , "  |   (  )   |"
      , "   \\   ''   /"
      , "    '._  _.'"
      , "       ||"
      ]
  , unlines
      [ "         _"
      , "      _.-'_`-."
      , "   .-'  |_|  '-."
      , "  /  Coffee?    \\"
      , " |  418: Nope!   |"
      , "  \\    Tea!     /"
      , "   '-._     _.-'"
      , "       '---'"
      ]
  ]

-- Get a random teapot
getRandomTeapot :: IO String
getRandomTeapot = do
  idx <- randomRIO (0, length teapots - 1)
  return $ teapots !! idx

-- Check if request is coffee-related (RFC2324)
isCoffeeRequest :: Request -> Bool
isCoffeeRequest req =
  let method = requestMethod req
      headers = requestHeaders req
      acceptAdditions = lookup "Accept-Additions" headers
      contentType = lookup "Content-Type" headers
  in method == "BREW" ||
     method == "WHEN" ||
     (fmap (BS.isInfixOf "coffee") acceptAdditions == Just True) ||
     (fmap (BS.isInfixOf "Coffee") acceptAdditions == Just True) ||
     (fmap (BS.isInfixOf "coffee") contentType == Just True)

-- Middleware to handle custom HTTP methods (ACQUIRE, VALIDATE, ABDICATE, HELP) and RFC2324
customMethodMiddleware :: Connection -> Int -> Int -> Middleware
customMethodMiddleware conn imageSize jpgQuality app req respond = do
  let method = requestMethod req
      path = rawPathInfo req

  -- Handle all custom methods explicitly
  case () of
    _ | method == "HELP" -> handleHelp req respond
      | method == "ACQUIRE" -> handleAcquire conn imageSize jpgQuality req respond
      | method == "BREW" -> handleTeapot req respond
      | method == "WHEN" -> handleTeapot req respond
      | method == "VALIDATE" && "/" `BS.isPrefixOf` path -> handleValidate conn req respond
      | method == "ABDICATE" && "/" `BS.isPrefixOf` path -> handleAbdicate conn req respond
      | isCoffeeRequest req -> handleTeapot req respond
      | otherwise -> app req respond

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

-- HELP handler - returns README.md
handleHelp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleHelp req respond = do
  result <- E.try $ BS.readFile "README.md"
  case result of
    Left (e :: E.SomeException) ->
      respond $ responseLBS status500 [("Content-Type", "text/plain")] "README.md not found"
    Right content ->
      respond $ responseLBS status200 [("Content-Type", "text/markdown")] (BL.fromStrict content)

-- Teapot handler - RFC2324 (I'm a teapot)
handleTeapot :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleTeapot req respond = do
  teapot <- getRandomTeapot
  respond $ responseLBS (mkStatus 418 "I'm a teapot")
    [("Content-Type", "text/plain; charset=utf-8")]
    (BL.fromStrict $ TE.encodeUtf8 $ T.pack teapot)

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
