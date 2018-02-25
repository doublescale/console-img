{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import Data.Monoid
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> putImageFile fileName
    _ -> do
      putStrLn "Give me one picture file as an argument."
      exitFailure

putImageFile :: FilePath -> IO ()
putImageFile fileName = do
  eitherImage <- readImage fileName
  case eitherImage of
    Right dynImage -> BS.putStr (BS.toLazyByteString (renderImage dynImage))
    Left errString -> do
      putStrLn errString
      exitFailure

renderImage :: DynamicImage -> BS.Builder
renderImage dynImage =
  mconcat
    [ mconcat
        [ ansiColoredHalfBlocks (paddedAt x y) (paddedAt x (y + 1))
        | x <- [0 .. imageWidth - 1]
        ]
      <> "\ESC[0m\n"
    | y <- [0,2 .. imageHeight - 1]
    ]
  where
    img@Image {imageHeight, imageWidth} = convertRGB8 dynImage
    paddedAt x y
      | y < imageHeight = pixelAt img x y
      | otherwise = PixelRGB8 0 0 0

ansiColoredHalfBlocks :: PixelRGB8 -> PixelRGB8 -> BS.Builder
ansiColoredHalfBlocks topCol botCol =
  "\ESC[48;2;" <> triple topCol <> ";38;2;" <> triple botCol <> "m\9604"
  where
    triple (PixelRGB8 r g b) =
      mconcat
        [ BS.word8Dec r
        , BS.charUtf8 ';'
        , BS.word8Dec g
        , BS.charUtf8 ';'
        , BS.word8Dec b
        ]
