{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
import Lift

main :: IO ()
main = ownAnimate (InWindow "Nice Window" (200, 200) (10, 10)) white behDraw 100
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
unlift0 :: Behavior Picture -> Float -> Picture
unlift0 b t=
    case b of
        Behavior y ->
            case y t of
                (a, Behavior z) -> a

ownAnimate d c b t =
   aux 0 where aux acc = if acc==t then display d c (unlift0 b acc) else do
                                                                         putStrLn "first test"
                                                                         display d c (unlift0 b acc)
                                                                         putStrLn "test"
                                                                         aux (acc+1)

