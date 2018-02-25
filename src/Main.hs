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
    Right dynImage -> putStr (renderTransposedMatrix (imageToMatrix dynImage))
    Left errString -> do
      putStrLn errString
      exitFailure

imageToMatrix :: DynamicImage -> [[PixelRGB8]]
imageToMatrix dynImage =
  [[pixelAt img x y | y <- [0 .. imageHeight - 1]] | x <- [0 .. imageWidth - 1]]
  where
    img@Image {imageHeight, imageWidth} = convertRGB8 dynImage

renderTransposedMatrix :: [[PixelRGB8]] -> String
renderTransposedMatrix =
  unlines . map ((++ "\ESC[0m") . concat) . transpose . map colorPairwise
  where
    colorPairwise (a:b:xs) = [ansiColoredHalfBlocks a b] ++ colorPairwise xs
    colorPairwise [a] = [ansiColoredHalfBlocks a (PixelRGB8 0 0 0)]
    colorPairwise [] = []

ansiColoredHalfBlocks :: PixelRGB8 -> PixelRGB8 -> String
ansiColoredHalfBlocks topCol botCol =
  "\ESC[48;2;" ++ triple topCol ++ ";38;2;" ++ triple botCol ++ "m\9604"
  where
    triple (PixelRGB8 r g b) = intercalate ";" (map show [r, g, b])
