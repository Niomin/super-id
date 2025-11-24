{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Events
  ( IdentityEvent(..)
  , IdentityAggregate(..)
  , emptyAggregate
  , applyEvent
  ) where

import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..), SumEncoding(..), genericToJSON, genericParseJSON, defaultOptions)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

-- | Domain events representing state changes in the Identity aggregate
data IdentityEvent
  = IdentityAcquired
      { acquiredAppId :: T.Text
      , acquiredPayload :: T.Text
      , acquiredFormat :: T.Text
      , acquiredAt :: UTCTime
      }
  | IdentityValidated
      { validatedAppId :: T.Text
      , validatedPayload :: Maybe T.Text
      , validationSuccessful :: Bool
      , validatedAt :: UTCTime
      }
  | IdentityAbdicated
      { abdicatedAppId :: T.Text
      , abdicatedAt :: UTCTime
      }
  deriving (Show, Eq, Generic)

-- Custom Aeson options for sum type encoding with "tag" field
eventOptions :: Options
eventOptions = defaultOptions
  { sumEncoding = TaggedObject
      { tagFieldName = "tag"
      , contentsFieldName = "contents"
      }
  }

instance ToJSON IdentityEvent where
  toJSON = genericToJSON eventOptions

instance FromJSON IdentityEvent where
  parseJSON = genericParseJSON eventOptions

-- | Identity aggregate state (rebuilt from events)
data IdentityAggregate = IdentityAggregate
  { aggUuid :: Maybe UUID
  , aggAppId :: Maybe T.Text
  , aggPayload :: Maybe T.Text
  , aggCreatedAt :: Maybe UTCTime
  , aggIsAbdicated :: Bool
  , aggValidationCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON IdentityAggregate
instance FromJSON IdentityAggregate

-- | Empty aggregate state
emptyAggregate :: IdentityAggregate
emptyAggregate = IdentityAggregate
  { aggUuid = Nothing
  , aggAppId = Nothing
  , aggPayload = Nothing
  , aggCreatedAt = Nothing
  , aggIsAbdicated = False
  , aggValidationCount = 0
  }

-- | Apply an event to an aggregate to produce new state
applyEvent :: IdentityAggregate -> IdentityEvent -> IdentityAggregate
applyEvent agg (IdentityAcquired appId payload _ createdAt) =
  agg { aggAppId = Just appId
      , aggPayload = Just payload
      , aggCreatedAt = Just createdAt
      }
applyEvent agg (IdentityValidated _ _ _ _) =
  agg { aggValidationCount = aggValidationCount agg + 1 }
applyEvent agg (IdentityAbdicated _ _) =
  agg { aggIsAbdicated = True }
