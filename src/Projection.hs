{-# LANGUAGE OverloadedStrings #-}

module Projection
  ( projectToReadModel
  ) where

import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG

import Events

-- | Project an event to the read model (synchronous projection)
-- This updates the identities table based on events
projectToReadModel :: Pool Connection -> UUID -> IdentityEvent -> IO ()
projectToReadModel pool aggregateId event = withResource pool $ \conn -> case event of
  IdentityAcquired appId payload _ timestamp -> do
    -- Insert into read model
    _ <- PG.execute conn
      "INSERT INTO identities (uuid, app_id, payload_base64, created_at) VALUES (?, ?, ?, ?) \
      \ON CONFLICT (uuid) DO UPDATE SET app_id = EXCLUDED.app_id, payload_base64 = EXCLUDED.payload_base64"
      (aggregateId, appId, payload, timestamp)
    return ()

  IdentityValidated _ _ _ _ -> do
    -- Validation events don't change the read model, just audit trail
    return ()

  IdentityAbdicated _ _ -> do
    -- Delete from read model
    _ <- PG.execute conn
      "DELETE FROM identities WHERE uuid = ?"
      (PG.Only aggregateId)
    return ()
