module Main(main) where

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Gloss" (700,700) (0,0))
           white drawPicture



circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs(5*sin (number/10))
