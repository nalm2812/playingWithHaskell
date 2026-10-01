{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
import Lift 
main :: IO ()
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (wiggleRange 0 10)
--ownAnimate (InWindow "Nice Window" (200, 200) (10, 10)) white behDraw 100
--printBeh laterPipingB
--display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw

circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
behDraw :: Behavior Picture
value number =abs(5*sin (number/5))
behDraw = let test b t = (scale (value t) (value t) circleDraw, b) in Behavior(test behDraw)
ownAnimate :: Display -> Color -> Behavior Picture -> Float -> IO ()

ownAnimate d c b t =
   aux 0 where aux acc = if acc==t then display d c (unlift0 b acc) else do
                                                                         putStrLn "first test"
                                                                         display d c (unlift0 b acc)
                                                                         putStrLn "test"
                                                                         aux (acc+1)
wiggle :: Float -> Float
wiggle t = sin (pi*t)
wiggleRange :: Float -> Float -> Float -> Picture
wiggleRange hi lo t = translate 0 (lo + (hi-lo) * (wiggle t+1)/2) circleDraw

