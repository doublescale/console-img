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
    Right dynImage -> putStr (renderRgbMatrix (imageToMatrix dynImage))
    Left errString -> do
      putStrLn errString
      exitFailure

imageToMatrix :: DynamicImage -> [[PixelRGB8]]
imageToMatrix dynImage =
  [[pixelAt img x y | x <- [0 .. imageWidth - 1]] | y <- [0 .. imageHeight - 1]]
  where
    img@Image {imageHeight, imageWidth} = convertRGB8 dynImage

renderRgbMatrix :: [[PixelRGB8]] -> String
renderRgbMatrix =
  unlines . map concat . transpose . map colorPairwise . transpose
  where
    colorPairwise (a:b:xs) = [ansiColoredHalfBlocks a b] ++ colorPairwise xs
    colorPairwise [a] = [ansiColoredHalfBlocks a (PixelRGB8 0 0 0)]
    colorPairwise [] = []

ansiColoredHalfBlocks :: PixelRGB8 -> PixelRGB8 -> String
ansiColoredHalfBlocks topCol botCol =
  "\ESC[48;2;" ++ triple topCol ++ ";38;2;" ++ triple botCol ++ "m\9604\ESC[0m"
  where
    triple (PixelRGB8 r g b) = intercalate ";" (map show [r, g, b])
