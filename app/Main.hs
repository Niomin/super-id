{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import Text.Read (readMaybe)

import App
import EventStore

main :: IO ()
main = do
  -- Set unbuffered output for logging
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Initialize connection pool for event store
  connPool <- initConnectionPool
  putStrLn "Connection pool created"

  -- Initialize event store
  eventStore <- initEventStore connPool
  putStrLn "Event store initialized (YOLO mode activated)"

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
  app <- mkApp connPool eventStore imageSize jpgQuality
  run 3000 app
