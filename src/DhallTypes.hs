{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DhallTypes
  ( AcquireRequest(..)
  , ValidateRequest(..)
  , AbdicateRequest(..)
  , PayloadType(..)
  , parseDhallAcquireRequest
  , parseDhallValidateRequest
  , parseDhallAbdicateRequest
  , payloadToBase64
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import Dhall (Generic, FromDhall, input, auto)
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)

data PayloadType = PayloadType
  { teamId :: Integer
  } deriving (Show, Generic, FromDhall)

data AcquireRequest = AcquireRequest
  { appId :: T.Text
  , payload :: PayloadType
  } deriving (Show, Generic, FromDhall)

data ValidateRequest = ValidateRequest
  { valAppId :: T.Text
  , valPayload :: Maybe PayloadType
  } deriving (Show, Generic, FromDhall)

data AbdicateRequest = AbdicateRequest
  { abdAppId :: T.Text
  , dynamicPromise :: Bool
  } deriving (Show, Generic)

-- Parse Dhall ACQUIRE request
parseDhallAcquireRequest :: T.Text -> IO (Either String AcquireRequest)
parseDhallAcquireRequest dhallText = do
  result <- try $ input auto dhallText
  case result of
    Left (e :: SomeException) -> return $ Left (show e)
    Right val -> return $ Right val

-- Parse Dhall VALIDATE request
parseDhallValidateRequest :: T.Text -> IO (Either String ValidateRequest)
parseDhallValidateRequest dhallText = do
  result <- try $ input auto dhallText
  case result of
    Left (e :: SomeException) -> return $ Left (show e)
    Right val -> return $ Right val

-- Parse Dhall ABDICATE request with dynamic field checking
-- Uses simple text parsing to extract appId and check for dynamic promise field
parseDhallAbdicateRequest :: T.Text -> IO (Either String AbdicateRequest)
parseDhallAbdicateRequest dhallText = do
  result <- try $ do
    -- Extract appId using simple text search
    let appIdMatch = T.breakOn "appId = \"" dhallText
    if T.null (snd appIdMatch)
      then return $ Left "appId field not found"
      else do
        let afterAppId = T.drop 10 (snd appIdMatch)  -- skip 'appId = "'
            appIdVal = T.takeWhile (/= '\"') afterAppId
            promiseFieldName = "iPromiseAppId" <> appIdVal <> "IsMine"
            hasPromiseTrue = (promiseFieldName <> " = true") `T.isInfixOf` dhallText ||
                           (promiseFieldName <> " = True") `T.isInfixOf` dhallText
            hasPromiseFalse = (promiseFieldName <> " = false") `T.isInfixOf` dhallText ||
                            (promiseFieldName <> " = False") `T.isInfixOf` dhallText
        if hasPromiseTrue
          then return $ Right $ AbdicateRequest appIdVal True
          else if hasPromiseFalse
                 then return $ Right $ AbdicateRequest appIdVal False
                 else return $ Left "Promise field not found or invalid"
  case result of
    Left (e :: SomeException) -> return $ Left (show e)
    Right res -> return res

-- Convert payload to base64
payloadToBase64 :: PayloadType -> T.Text
payloadToBase64 p = TE.decodeUtf8 $ B64.encode $ TE.encodeUtf8 $ T.pack $ show (teamId p)
