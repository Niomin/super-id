{-# LANGUAGE OverloadedStrings #-}

module AudioGen
  ( generateWAV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import System.Process
import System.IO.Temp
import System.Directory
import System.IO (hClose)

-- Generate WAV file with robotic voice reading UUID
generateWAV :: UUID -> IO BL.ByteString
generateWAV uuid = do
  let uuidStr = UUID.toString uuid
      -- Convert UUID to speech-friendly format (spell out each character with spaces)
      speechText = unwords $ map (\c -> if c == '-' then "dash" else [c]) uuidStr

  -- Create temporary file for WAV output
  withSystemTempFile "uuid.wav" $ \tmpPath tmpHandle -> do
    -- Close the handle as espeak will write to the file
    hClose tmpHandle

    -- Use espeak-ng to generate WAV file
    -- -v en: English voice
    -- -s 150: Speed (words per minute)
    -- -w: Write output to WAV file
    let args = ["-v", "en", "-s", "120", "-w", tmpPath, speechText]
    _ <- readProcessWithExitCode "espeak-ng" args ""

    -- Read the generated WAV file
    BL.readFile tmpPath
