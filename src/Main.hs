module Main where

import Codec.Picture
import Data.List
import GHC.Word

main :: IO ()
main = return ()

data RGB888 = RGB888 Word8 Word8 Word8 deriving (Show)

ansiColoredHalfBlocks :: RGB888 -> RGB888 -> String
ansiColoredHalfBlocks topCol botCol =
  "\ESC[48;2;" ++ triple topCol ++ ";38;2;" ++ triple botCol ++ "m\9604\ESC[0m"
  where
    triple (RGB888 r g b) = intercalate ";" (map show [r, g, b])

renderRgbMatrix :: [[RGB888]] -> String
renderRgbMatrix =
  unlines . map concat . transpose . map colorPairwise . transpose
  where
    colorPairwise (a:b:xs) = [ansiColoredHalfBlocks a b] ++ colorPairwise xs
    colorPairwise [a] = [ansiColoredHalfBlocks a (RGB888 0 0 0)]
    colorPairwise [] = []
