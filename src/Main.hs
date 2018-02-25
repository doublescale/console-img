{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Codec.Picture
import Data.List
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
    Right dynImage -> putStr (renderImage dynImage)
    Left errString -> do
      putStrLn errString
      exitFailure

renderImage :: DynamicImage -> String
renderImage dynImage =
  unlines
    [ concat
        [ ansiColoredHalfBlocks (paddedAt x y) (paddedAt x (y + 1))
        | x <- [0 .. imageWidth - 1]
        ]
      ++ "\ESC[0m"
    | y <- [0,2 .. imageHeight - 1]
    ]
  where
    img@Image {imageHeight, imageWidth} = convertRGB8 dynImage
    paddedAt x y
      | y < imageHeight = pixelAt img x y
      | otherwise = PixelRGB8 0 0 0

ansiColoredHalfBlocks :: PixelRGB8 -> PixelRGB8 -> String
ansiColoredHalfBlocks topCol botCol =
  "\ESC[48;2;" ++ triple topCol ++ ";38;2;" ++ triple botCol ++ "m\9604"
  where
    triple (PixelRGB8 r g b) = intercalate ";" (map show [r, g, b])
