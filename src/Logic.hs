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
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import DhallTypes
import qualified ImageGen
import qualified AudioGen
import EventStore
import Events
import Projection

data ResponseFormat
  = FormatPNG
  | FormatJPG
  | FormatGIF
  | FormatWAV
  deriving (Show, Eq)

-- ACQUIRE logic: generate UUID, persist, return in requested format
acquireIdentity :: Pool Connection -> EventStoreConfig -> AcquireRequest -> ResponseFormat -> Int -> Int -> IO (UUID, BL.ByteString, T.Text)
acquireIdentity pool eventStore req format imageSize jpgQuality = do
  -- Generate new UUID
  newUuid <- UUID.nextRandom

  -- Convert payload to base64
  let payloadB64 = payloadToBase64 (payload req)

  -- EVENT SOURCING: Store the event (source of truth)
  now <- getCurrentTime
  let event = IdentityAcquired
        { acquiredAppId = appId req
        , acquiredPayload = payloadB64
        , acquiredFormat = formatToText format
        , acquiredAt = now
        }
  storeEvent eventStore newUuid event

  -- PROJECT: Update read model from event
  projectToReadModel pool newUuid event

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
validateIdentityLogic :: Pool Connection -> EventStoreConfig -> UUID -> ValidateRequest -> IO Bool
validateIdentityLogic pool eventStore uuid req = do
  let maybePayloadB64 = fmap payloadToBase64 (valPayload req)

  -- REBUILD from events (source of truth)
  result <- aggregateExists eventStore uuid (valAppId req) maybePayloadB64

  -- EVENT SOURCING: Store validation event
  now <- getCurrentTime
  let event = IdentityValidated
        { validatedAppId = valAppId req
        , validatedPayload = maybePayloadB64
        , validationSuccessful = result
        , validatedAt = now
        }
  storeEvent eventStore uuid event
  -- Note: Validation events don't update read model (audit only)

  return result

-- ABDICATE logic: delete identity if appId matches and promise is correct
abdicateIdentity :: Pool Connection -> EventStoreConfig -> UUID -> AbdicateRequest -> IO (Either String ())
abdicateIdentity pool eventStore uuid req = do
  -- Check if dynamic promise field is correct
  -- The field name should be: iPromiseAppId{appId}IsMine
  if not (dynamicPromise req)
    then return $ Left "Promise field is false or missing"
    else do
      -- REBUILD from events to check if it exists
      agg <- getAggregate eventStore uuid
      case (aggAppId agg, aggIsAbdicated agg) of
        (Just actualAppId, False) | actualAppId == abdAppId req -> do
          -- EVENT SOURCING: Store abdication event (source of truth)
          now <- getCurrentTime
          let event = IdentityAbdicated
                { abdicatedAppId = abdAppId req
                , abdicatedAt = now
                }
          storeEvent eventStore uuid event

          -- PROJECT: Update read model
          projectToReadModel pool uuid event

          return $ Right ()
        _ -> return $ Left "Not found"

-- Helper to convert format to text
formatToText :: ResponseFormat -> T.Text
formatToText FormatPNG = "PNG"
formatToText FormatJPG = "JPG"
formatToText FormatGIF = "GIF"
formatToText FormatWAV = "WAV"
