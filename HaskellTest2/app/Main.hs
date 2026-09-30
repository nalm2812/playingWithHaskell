{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss

main :: IO ()
main = printBeh laterPipingB
--display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw


data Behavior a = Behavior (Float -> (a, Behavior a))

circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
value number =abs (5*sin (number/5 ))
circleAnimation :: Behavior Picture
testFunction b number = (scale (value number) (value number) circleDraw,b)
circleAnimation = Behavior (testFunction circleAnimation)



add :: Integer -> Integer
add x = x+x
liftA :: (a->a) -> (Behavior a -> Behavior a)
liftA f x=
    case x of
        Behavior y -> case y 0 of
                        (a, Behavior z) ->
                            let test b (huh::Float) = (f a, b); idk = Behavior (test idk) in idk

intBeh2 :: Behavior Integer
intBeh2 = let test b number = (2, b) in Behavior (test intBeh2)
printBeh :: Behavior String -> IO ()
printBeh b =
    case b of
        Behavior y ->
            case y 0 of
                (a, _) -> print a

laterPiping = liftA add intBeh2

liftB :: (a->b) -> (Behavior a -> Behavior b)
liftB f x =
    case x of
            Behavior y -> case y 0 of
                            (a, Behavior z) ->
                                let test b (huh::Float) = (f a, b); idk = Behavior (test idk) in idk
evenOrOdd :: Integer -> String
evenOrOdd x = if even x then "Even" else "Odd"
laterPipingB = liftB evenOrOdd intBehMaybe
intBeh3 = let test b number = (3, b) in Behavior (test intBeh2)

intBehMaybe = let test b number = if number == 0 then (3, b) else (4,b) in Behavior (test intBehMaybe)







--ownAnimate :: Display -> Color -> Behavior Picture
