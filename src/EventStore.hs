{-# LANGUAGE OverloadedStrings #-}

module EventStore
  ( EventStoreConfig(..)
  , initEventStore
  , initConnectionPool
  , storeEvent
  , loadEvents
  , rebuildAggregate
  , getAggregate
  , aggregateExists
  , IdentityStreamId
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Pool (Pool, withResource, createPool)
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), defaultConnectInfo, connect, close)
import qualified Database.PostgreSQL.Simple as PG
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import System.Environment (getEnv)

import Events

-- | Configuration for the event store
data EventStoreConfig = EventStoreConfig
  { esConnPool :: Pool Connection
  , esStreamName :: T.Text
  }

-- | Stream identifier for Identity aggregates
type IdentityStreamId = UUID

-- | Initialize connection pool for event store
initConnectionPool :: IO (Pool Connection)
initConnectionPool = do
  host <- getEnv "POSTGRES_HOST"
  port <- read <$> getEnv "POSTGRES_PORT"
  user <- getEnv "POSTGRES_USER"
  password <- getEnv "POSTGRES_PASSWORD"
  database <- getEnv "POSTGRES_DB"

  let connInfo = defaultConnectInfo
        { connectHost = host
        , connectPort = port
        , connectUser = user
        , connectPassword = password
        , connectDatabase = database
        }

  createPool (connect connInfo) close 1 60 10

-- | Initialize event store with PostgreSQL backend
initEventStore :: Pool Connection -> IO EventStoreConfig
initEventStore pool = do
  return $ EventStoreConfig
    { esConnPool = pool
    , esStreamName = "identity_stream"
    }

-- | Store an event for a specific aggregate
storeEvent :: EventStoreConfig -> UUID -> IdentityEvent -> IO ()
storeEvent config aggregateId event = do
  let streamId = UUID.toText aggregateId
  let eventData = encode event

  -- Store event in PostgreSQL
  withResource (esConnPool config) $ \conn -> do
    _ <- PG.execute conn
      "INSERT INTO events (stream_id, event_type, event_data, created_at) VALUES (?, ?, ?, NOW())"
      (streamId, eventTypeName event, eventData)
    return ()

-- | Load all events for a specific aggregate
loadEvents :: EventStoreConfig -> UUID -> IO [IdentityEvent]
loadEvents config aggregateId = do
  let streamId = UUID.toText aggregateId
  putStrLn $ "DEBUG: Loading events for stream_id: " ++ show streamId

  withResource (esConnPool config) $ \conn -> do
    rows <- PG.query conn
      "SELECT event_data::text FROM events WHERE stream_id = ? ORDER BY created_at ASC"
      (PG.Only streamId) :: IO [PG.Only T.Text]

    putStrLn $ "DEBUG: Loaded " ++ show (length rows) ++ " event rows"
    let events = mapMaybe (decode . BL.fromStrict . TE.encodeUtf8 . PG.fromOnly) rows
    putStrLn $ "DEBUG: Decoded " ++ show (length events) ++ " events"
    putStrLn $ "DEBUG: Events: " ++ show events
    return events

-- | Rebuild aggregate from events
rebuildAggregate :: [IdentityEvent] -> IdentityAggregate
rebuildAggregate = foldl applyEvent emptyAggregate

-- | Get current aggregate state by replaying events
getAggregate :: EventStoreConfig -> UUID -> IO IdentityAggregate
getAggregate config aggregateId = do
  events <- loadEvents config aggregateId
  let agg = rebuildAggregate events
  return $ agg { aggUuid = Just aggregateId }

-- | Check if aggregate exists and matches criteria
aggregateExists :: EventStoreConfig -> UUID -> T.Text -> Maybe T.Text -> IO Bool
aggregateExists config aggregateId appId maybePayload = do
  agg <- getAggregate config aggregateId
  putStrLn $ "DEBUG: Aggregate state: " ++ show agg
  putStrLn $ "DEBUG: Expected appId: " ++ show appId
  putStrLn $ "DEBUG: Expected payload: " ++ show maybePayload
  case (aggAppId agg, aggIsAbdicated agg) of
    (Just actualAppId, False) ->
      if actualAppId == appId
        then case maybePayload of
          Nothing -> return True
          Just expectedPayload -> do
            let result = aggPayload agg == Just expectedPayload
            putStrLn $ "DEBUG: Payload comparison: " ++ show (aggPayload agg) ++ " == " ++ show (Just expectedPayload) ++ " = " ++ show result
            return result
        else do
          putStrLn $ "DEBUG: AppId mismatch: " ++ show actualAppId ++ " /= " ++ show appId
          return False
    _ -> do
      putStrLn $ "DEBUG: Invalid aggregate state - appId: " ++ show (aggAppId agg) ++ ", abdicated: " ++ show (aggIsAbdicated agg)
      return False

-- Helper functions
eventTypeName :: IdentityEvent -> T.Text
eventTypeName (IdentityAcquired _ _ _ _) = "IdentityAcquired"
eventTypeName (IdentityValidated _ _ _ _) = "IdentityValidated"
eventTypeName (IdentityAbdicated _ _) = "IdentityAbdicated"

-- Note: This is a simplified event store implementation using PostgreSQL directly.
-- For production use with the eventsourcing library, you would:
-- 1. Use EventSourcing.Store.PostgreSQL for backend
-- 2. Implement proper serialization with versioning
-- 3. Add snapshots for performance
-- 4. Implement proper error handling
-- 5. Use the library's aggregate store patterns
