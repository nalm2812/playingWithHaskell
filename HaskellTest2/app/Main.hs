module Main(main) where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw

data Behavior a =
    Behavior (Float -> (a, Behavior a))

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
result x f=
    case x of
        Behavior y -> case y 0 of
                        (a, Behavior z) ->
                            let test (b::Behavior Integer) (huh::Float) = (f a, b); idk = Behavior(test idk) in idk
                        _ -> let test (b::Behavior Integer) (huh::Float) = (0, b); idk = Behavior(test idk) in idk
        _ -> let test b huh = (0, b); idk = Behavior(test idk) in idk

--liftA :: (a->a) -> (Behavior a -> Behavior a)



--ownAnimate :: Display -> Color -> Behavior Picture
