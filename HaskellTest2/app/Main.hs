{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
main :: IO ()
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (wiggleRange 0 10)
--printBeh laterPipingB
--display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw
data Behavior a = Behavior (Float -> (a, Behavior a))

circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
behDraw :: Behavior Picture
value number =abs(5*sin (number/5))
behDraw = let test b t = (scale (value t) (value t) circleDraw, b) in Behavior(test behDraw)


wiggle :: Float -> Float
wiggle t = sin (pi*t)
wiggleRange :: Float -> Float -> Float -> Picture
wiggleRange hi lo t = translate 0 (lo + (hi-lo) * (wiggle t+1)/2) circleDraw


unlift0 :: Behavior Picture -> Float -> Picture
unlift0 b t=
    case b of
        Behavior y ->
            case y t of
                (a, Behavior z) -> a

