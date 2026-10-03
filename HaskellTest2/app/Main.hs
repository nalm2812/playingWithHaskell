{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
main :: IO ()
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (extractBeh behWiggleAnimation)
--printBeh laterPipingB
--display (InWindow "Nice Window" (200, 200) (10, 10)) white circleDraw
data Behavior a = Behavior (Float -> (a, Behavior a))

circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
--behDraw :: Behavior Picture
value :: Float -> Float -> Float
value number factor =abs(factor*sin (number/factor))
--behDraw = let test b t = (scale (value t) (value t) circleDraw, b) in Behavior(test behDraw)


wiggle :: Float -> Float
wiggle t = sin (pi*t)
wiggleRange :: Float -> Float -> Float -> Picture
wiggleRange hi lo t = translate 0 (lo + (hi-lo) * (wiggle t+1)/2) circleDraw

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f (Behavior fa) = Behavior fb where fb t = let(a,ba) = fa t in (f a, lift1 f ba)
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f (Behavior fa) (Behavior fb) = Behavior fc where fc t = let(a,ba) = fa t; (b,bb) = fb t in (f a b, lift2 f ba bb)
lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 f (Behavior fa) (Behavior fb) (Behavior fc) = Behavior fd where fd t = let(a,ba) = fa t; (b,bb) = fb t; (c, bc) = fc t in (f a b c, lift3 f ba bb bc)
lift4 :: (a -> b -> c -> d -> e) -> Behavior a -> Behavior b -> Behavior c -> Behavior d -> Behavior e
lift4 f (Behavior fa) (Behavior fb) (Behavior fc) (Behavior fd) = Behavior fe where fe t = let(a,ba) = fa t; (b,bb) = fb t; (c, bc) = fc t; (d,bd) = fd t in (f a b c d, lift4 f ba bb bc bd)
bscale :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bscale = lift3 scale
constant :: a -> Behavior a
constant a = Behavior ab where ab t = (a, constant a)
behCircle = constant circleDraw
extractBeh :: Behavior Picture -> (Float -> Picture)
extractBeh (Behavior b) = f where f t = let(a,ba)=b t in a

time :: Behavior Float
time = Behavior(\t -> (t, time))
behDraw = bscale (lift2 value time (constant 5)) (lift2 value time (constant 5)) behCircle
behWiggleMotion :: Behavior Float
behWiggleMotion = lift1 wiggle time
btranslate :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
btranslate = lift3 translate
behWiggleRange :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
behWiggleRange hi lo = btranslate (constant 0) (lift3 (\h l w-> l + (h-l) * (w+1)/2) hi lo behWiggleMotion)
behWiggleAnimation :: Behavior Picture
behWiggleAnimation = behWiggleRange (constant 20) (constant 0) behCircle