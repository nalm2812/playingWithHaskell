{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
import Lift

main :: IO ()
main = printBeh laterPipingB
--display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw



circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))











--ownAnimate :: Display -> Color -> Behavior Picture
