{-# LANGUAGE OverloadedStrings #-}

module Logic
  ( acquireIdentity
  , validateIdentityLogic
  , abdicateIdentity
  , ResponseFormat(..)
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection)
import Repository
import DhallTypes
import qualified ImageGen
import qualified AudioGen

data ResponseFormat
  = FormatPNG
  | FormatJPG
  | FormatGIF
  | FormatWAV
  deriving (Show, Eq)

-- ACQUIRE logic: generate UUID, persist, return in requested format
acquireIdentity :: Connection -> AcquireRequest -> ResponseFormat -> Int -> Int -> IO (UUID, BL.ByteString, T.Text)
acquireIdentity conn req format imageSize jpgQuality = do
  -- Generate new UUID
  newUuid <- UUID.nextRandom

  -- Convert payload to base64
  let payloadB64 = payloadToBase64 (payload req)

  -- Persist to database
  insertIdentity conn newUuid (appId req) payloadB64

  -- Generate response based on format
  (responseBytes, contentType) <- case format of
    FormatPNG -> do
      let png = ImageGen.generatePNG imageSize newUuid
      return (png, "image/png")
    FormatJPG -> do
      let jpg = ImageGen.generateJPG imageSize jpgQuality newUuid
      return (jpg, "image/jpeg")
    FormatGIF -> do
      let gif = ImageGen.generateGIF imageSize newUuid
      return (gif, "image/gif")
    FormatWAV -> do
      wav <- AudioGen.generateWAV newUuid
      return (wav, "audio/x-wav")

  return (newUuid, responseBytes, contentType)

-- VALIDATE logic: check if UUID exists with given appId and optional payload
validateIdentityLogic :: Connection -> UUID -> ValidateRequest -> IO Bool
validateIdentityLogic conn uuid req = do
  let maybePayloadB64 = fmap payloadToBase64 (valPayload req)
  validateIdentity conn uuid (valAppId req) maybePayloadB64

-- ABDICATE logic: delete identity if appId matches and promise is correct
abdicateIdentity :: Connection -> UUID -> AbdicateRequest -> IO (Either String ())
abdicateIdentity conn uuid req = do
  -- Check if dynamic promise field is correct
  -- The field name should be: iPromiseAppId{appId}IsMine
  if not (dynamicPromise req)
    then return $ Left "Promise field is false or missing"
    else do
      -- Try to delete
      deleted <- deleteIdentity conn uuid (abdAppId req)
      if deleted
        then return $ Right ()
        else return $ Left "Not found"
