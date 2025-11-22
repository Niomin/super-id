{-# LANGUAGE OverloadedStrings #-}

module Repository
  ( initDB
  , insertIdentity
  , validateIdentity
  , deleteIdentity
  , Identity(..)
  ) where

import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import System.Environment (getEnv)

data Identity = Identity
  { identityUuid :: UUID
  , identityAppId :: T.Text
  , identityPayloadBase64 :: T.Text
  } deriving (Show, Eq)

instance FromRow Identity where
  fromRow = Identity <$> field <*> field <*> field

-- Initialize database connection
initDB :: IO Connection
initDB = do
  host <- getEnv "POSTGRES_HOST"
  port <- read <$> getEnv "POSTGRES_PORT"
  user <- getEnv "POSTGRES_USER"
  password <- getEnv "POSTGRES_PASSWORD"
  database <- getEnv "POSTGRES_DB"

  connect defaultConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = user
    , connectPassword = password
    , connectDatabase = database
    }

-- Insert a new identity
insertIdentity :: Connection -> UUID -> T.Text -> T.Text -> IO ()
insertIdentity conn uuid appId payloadBase64 = do
  _ <- execute conn
    "INSERT INTO identities (uuid, app_id, payload_base64) VALUES (?, ?, ?)"
    (uuid, appId, payloadBase64)
  return ()

-- Validate identity with optional payload check
validateIdentity :: Connection -> UUID -> T.Text -> Maybe T.Text -> IO Bool
validateIdentity conn uuid appId maybePayload = do
  results <- case maybePayload of
    Nothing -> query conn
      "SELECT uuid, app_id, payload_base64 FROM identities WHERE uuid = ? AND app_id = ?"
      (uuid, appId) :: IO [Identity]
    Just payload -> query conn
      "SELECT uuid, app_id, payload_base64 FROM identities WHERE uuid = ? AND app_id = ? AND payload_base64 = ?"
      (uuid, appId, payload) :: IO [Identity]
  return $ not (null results)

-- Delete identity
deleteIdentity :: Connection -> UUID -> T.Text -> IO Bool
deleteIdentity conn uuid appId = do
  rowsAffected <- execute conn
    "DELETE FROM identities WHERE uuid = ? AND app_id = ?"
    (uuid, appId)
  return $ rowsAffected > 0
