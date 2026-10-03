{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
main :: IO ()
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (extractBeh behRhombuSpin)
data Behavior a = Behavior (Float -> (a, Behavior a))

circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
--behDraw :: Behavior Picture
value :: Float -> Float -> Float
value number factor =abs (factor*sin (number/factor))
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
constant :: a -> Behavior a
constant a = Behavior ab where ab t = (a, constant a)

bscale :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bscale = lift3 scale
behCircle :: Behavior Float -> Behavior Picture
behCircle = lift1 Circle
behCircle80 = constant circleDraw
extractBeh :: Behavior Picture -> (Float -> Picture)
extractBeh (Behavior b) = f where f t = let(a,ba)=b t in a

time :: Behavior Float
time = Behavior (\t -> (t, time))


behZoom :: Behavior Float -> Behavior Picture -> Behavior Picture
behZoom factor = bscale (lift2 value time factor) (lift2 value time factor)
behCircleZoom :: Behavior Picture
behCircleZoom = behZoom (constant 5) behCircle80


behWiggleMotion :: Behavior Float
behWiggleMotion = lift1 wiggle time
btranslate ::  Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
btranslate = lift3 translate
behWiggleRange :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
behWiggleRange hi lo wiggle= btranslate (constant 0) (lift3 (\h l w-> l + (h-l) * (w+1)/2) hi lo wiggle)
behWiggleAnimationLocation :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
behWiggleAnimationLocation x wiggle factor = btranslate x (constant 0) (behWiggleRange factor (constant 0) wiggle behCircle80)
behWiggleAnimationLocationSin :: Behavior Float -> Behavior Float -> Behavior Picture
behWiggleAnimationLocationSin x = behWiggleAnimationLocation x behWiggleMotion


addPicture :: Behavior Picture -> Behavior [Picture] -> Behavior [Picture]
addPicture = lift2 (:)
bpictures :: Behavior [Picture] -> Behavior Picture
emptyList :: Behavior [Picture]
emptyList = constant []
bpictures = lift1 pictures
twoWiggle :: Behavior Picture
twoWiggle = bpictures (addPicture (behWiggleAnimationLocationSin (constant (-100)) (constant 100)) (addPicture (behWiggleAnimationLocationCos (constant 100) (constant 100)) emptyList))

cosWiggleMotion :: Float -> Float
cosWiggleMotion t = cos (pi*t)
behWiggleMotionCos :: Behavior Float
behWiggleMotionCos = lift1 cosWiggleMotion time
behWiggleAnimationLocationCos :: Behavior Float -> Behavior Float -> Behavior Picture
behWiggleAnimationLocationCos x = behWiggleAnimationLocation x behWiggleMotionCos

behRhombus :: Behavior Float -> Behavior Picture
rhombus :: Float -> Picture
rhombus r = Polygon [(-r,0),(0,r),(r,0),(0,-r),(-r,0)]
behRhombus= lift1 rhombus
behRhombus50 :: Behavior Picture
behRhombus50 = behRhombus (constant 50)


behRhombusZoom :: Behavior Picture
behRhombusZoom = behZoom (constant 5) behRhombus50
bRotate :: Behavior Float -> Behavior Picture -> Behavior Picture
bRotate = lift2 rotate --should i add time here?
behRotateFactor :: Behavior Float -> Behavior Picture -> Behavior Picture
behRotateFactor factor = bRotate (lift2 (*) factor time)
behRhombuSpin :: Behavior Picture
behRhombuSpin = behRotateFactor (constant 10) behRhombus50

