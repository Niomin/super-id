{-# LANGUAGE OverloadedStrings #-}

module ImageGen
  ( generatePNG
  , generateJPG
  , generateGIF
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Codec.Picture
import Codec.Picture.Types
import Data.Word (Word8)

-- Simple 5x5 font for digits and letters (for UUID)
-- Each character is represented as a list of 5 rows, each row is a list of bools
digitFont :: Char -> [[Bool]]
digitFont '0' = [[True,True,True],[True,False,True],[True,False,True],[True,False,True],[True,True,True]]
digitFont '1' = [[False,True,False],[True,True,False],[False,True,False],[False,True,False],[True,True,True]]
digitFont '2' = [[True,True,True],[False,False,True],[True,True,True],[True,False,False],[True,True,True]]
digitFont '3' = [[True,True,True],[False,False,True],[True,True,True],[False,False,True],[True,True,True]]
digitFont '4' = [[True,False,True],[True,False,True],[True,True,True],[False,False,True],[False,False,True]]
digitFont '5' = [[True,True,True],[True,False,False],[True,True,True],[False,False,True],[True,True,True]]
digitFont '6' = [[True,True,True],[True,False,False],[True,True,True],[True,False,True],[True,True,True]]
digitFont '7' = [[True,True,True],[False,False,True],[False,False,True],[False,False,True],[False,False,True]]
digitFont '8' = [[True,True,True],[True,False,True],[True,True,True],[True,False,True],[True,True,True]]
digitFont '9' = [[True,True,True],[True,False,True],[True,True,True],[False,False,True],[True,True,True]]
digitFont 'a' = [[False,True,False],[True,False,True],[True,True,True],[True,False,True],[True,False,True]]
digitFont 'b' = [[True,True,False],[True,False,True],[True,True,False],[True,False,True],[True,True,False]]
digitFont 'c' = [[False,True,True],[True,False,False],[True,False,False],[True,False,False],[False,True,True]]
digitFont 'd' = [[True,True,False],[True,False,True],[True,False,True],[True,False,True],[True,True,False]]
digitFont 'e' = [[True,True,True],[True,False,False],[True,True,True],[True,False,False],[True,True,True]]
digitFont 'f' = [[True,True,True],[True,False,False],[True,True,False],[True,False,False],[True,False,False]]
digitFont '-' = [[False,False,False],[False,False,False],[True,True,True],[False,False,False],[False,False,False]]
digitFont _   = [[False,False,False],[False,False,False],[False,False,False],[False,False,False],[False,False,False]]

-- Generate a static PNG image with UUID text
generatePNG :: Int -> UUID -> BL.ByteString
generatePNG size uuid =
  let uuidStr = UUID.toString uuid
      scale = size `div` 12  -- Original was 12px, calculate scale factor
      width = length uuidStr * (4 * scale)
      height = size
      img = generateImage (pixelRenderer scale uuidStr) width height
  in encodePng img

-- Generate a static JPG image with UUID text (configurable quality)
generateJPG :: Int -> Int -> UUID -> BL.ByteString
generateJPG size quality uuid =
  let uuidStr = UUID.toString uuid
      scale = size `div` 12
      width = length uuidStr * (4 * scale)
      height = size
      imgRGB = generateImage (pixelRenderer scale uuidStr) width height :: Image PixelRGB8
      imgYCbCr = convertImage imgRGB :: Image PixelYCbCr8
  in encodeJpegAtQuality (fromIntegral quality) imgYCbCr

-- Pixel renderer for static images (scalable)
pixelRenderer :: Int -> String -> Int -> Int -> PixelRGB8
pixelRenderer scale uuidStr x y =
  let charIndex = x `div` (4 * scale)
      pixelInChar = (x `mod` (4 * scale)) `div` scale
      row = y `div` (scale * 2)  -- 5 rows fit in height with scaling
  in if charIndex < length uuidStr && pixelInChar < 3 && row < 5
     then
       let char = uuidStr !! charIndex
           font = digitFont char
           isOn = if row < length font && pixelInChar < length (font !! row)
                  then (font !! row) !! pixelInChar
                  else False
       in if isOn then PixelRGB8 0 0 0 else PixelRGB8 255 255 255
     else PixelRGB8 255 255 255

-- Generate an animated GIF with changing numbers for each UUID character
generateGIF :: Int -> UUID -> BL.ByteString
generateGIF size uuid =
  let uuidStr = UUID.toString uuid
      frames = map (generateFrame size uuidStr) [0..min 10 (length uuidStr - 1)]  -- Limit to 10 frames
      palOptions = PaletteOptions { paletteCreationMethod = MedianMeanCut
                                  , enableImageDithering = False
                                  , paletteColorCount = 256 }
  in if null frames
     then BL.empty
     else
       let firstPal = palettize palOptions (head frames)
       in case firstPal of
            (imgPal, pal) ->
              let palettedFrames = map (\f -> let (pf, _) = palettize palOptions f in pf) frames
                  gifImages = map (\pf -> (pal, 50, pf)) palettedFrames
              in case encodeGifImages LoopingForever gifImages of
                   Left _ -> BL.empty
                   Right gif -> gif

-- Generate a single frame for the GIF showing one character
generateFrame :: Int -> String -> Int -> Image PixelRGB8
generateFrame size uuidStr charIndex =
  let char = if charIndex < length uuidStr then uuidStr !! charIndex else ' '
      img = generateImage (framePixelRenderer size char) size size
  in img

-- Pixel renderer for GIF frames (scalable)
framePixelRenderer :: Int -> Char -> Int -> Int -> PixelRGB8
framePixelRenderer size char x y =
  let scale = size `div` 12
      font = digitFont char
      scaledX = (x - (4 * scale)) `div` scale  -- Center in size x size
      scaledY = (y - (3 * scale)) `div` scale
  in if scaledX >= 0 && scaledX < 3 && scaledY >= 0 && scaledY < 5
     then
       let isOn = if scaledY < length font && scaledX < length (font !! scaledY)
                  then (font !! scaledY) !! scaledX
                  else False
       in if isOn then PixelRGB8 0 0 0 else PixelRGB8 255 255 255
     else PixelRGB8 255 255 255
