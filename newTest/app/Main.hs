module Main(main) where

import Codec.Picture.Types
import Codec.Picture



main :: IO (Either String Codec.Picture.Types.DynamicImage)

main = readImage "bunny.jpg"
