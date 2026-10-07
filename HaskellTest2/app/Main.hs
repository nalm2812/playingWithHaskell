{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
main :: IO ()
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (at (execute testThreeWiggle))
data Behavior a = Behavior (Float -> (a, Behavior a))
data Event a = Event Float a
circleDraw :: Picture
circleDraw = Circle 80
drawPicture :: Float -> Picture
drawPicture number = scale value value circleDraw where value = abs (5*sin (number/5 ))
--behDraw :: Behavior Picture
value :: Float -> Float
value number =abs (sin number)
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
constant a = Behavior ab where ab _ = (a, constant a)

bscale :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bscale = lift3 scale
behCircle :: Behavior Float -> Behavior Picture
behCircle = lift1 Circle
behCircle80 = constant circleDraw
at :: Behavior a -> (Float -> a)
at (Behavior b) = f where f t = let(a,_)=b t in a

time :: Behavior Float
time = Behavior (\t -> (t, time))


behZoom :: Behavior Picture -> Behavior Picture
behZoom = bscale (lift1 value time) (lift1 value time)
behCircleZoom :: Behavior Picture
behCircleZoom = timeTransform (behZoom behCircle80) (time |* constant 5)


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
combinePicture :: Behavior Picture -> Behavior Picture -> Behavior Picture
combinePicture a b = bpictures (a |: (b |: emptyList))

(|:) :: Behavior Picture -> Behavior [Picture] -> Behavior [Picture]
(|:) = lift2 (:)
twoWiggleNew :: Behavior Picture
twoWiggleNew = bpictures (behWiggleAnimationLocationSin (constant (-100)) (constant 100) |: (behWiggleAnimationLocationCos (constant 100) (constant 100) |: emptyList))
wiggleOne = behWiggleAnimationLocationSin (constant (-100)) (constant 100)
wiggleTwo = behWiggleAnimationLocationCos (constant 100) (constant 100)
easierWiggle = bpictures (wiggleOne |: (wiggleTwo |: emptyList))

cosWiggleMotion :: Float -> Float
cosWiggleMotion t = cos (pi*t)
behWiggleMotionCos :: Behavior Float
behWiggleMotionCos = lift1 cosWiggleMotion time
behWiggleAnimationLocationCos :: Behavior Float -> Behavior Float -> Behavior Picture
behWiggleAnimationLocationCos x = behWiggleAnimationLocation x behWiggleMotionCos


behRhombus :: Behavior Float -> Behavior Picture
behRhombus n = bLine (lift1 (\r -> [(-r,0),(0,r),(r,0),(0,-r),(-r,0)]) n)
behRhombus50 :: Behavior Picture
behRhombus50 = behRhombus (constant 50)


behRhombusZoom :: Behavior Picture
behRhombusZoom = timeTransform (behZoom behRhombus50) (time |* constant 5)
bRotate :: Behavior Float -> Behavior Picture -> Behavior Picture
bRotate = lift2 rotate --should i add time here?
behSpin :: Behavior Picture -> Behavior Picture
behSpin = bRotate time
behRhombuSpin :: Behavior Picture
behRhombuSpin = behSpin (behRhombus (constant 50))


addPointToLine :: Behavior Point -> Behavior Path -> Behavior Path
addPointToLine = lift2 (:) --maybe change
growingLineFactor :: Behavior Float -> Behavior Picture
growingLineFactor = lift2 (\t f -> Line [(0,0), (f*t, 0)]) time
growingLine :: Behavior Picture
growingLine = growingLineFactor (constant 10)
bLine :: Behavior Path -> Behavior Picture
bLine = lift1 line

timeTransform :: Behavior a -> Behavior Float -> Behavior a
timeTransform (Behavior b) t2 = Behavior c where c t = let(a, ab) = b (at t2 t) in (a, timeTransform ab t2)
test = timeTransform behRhombuSpin (time |* constant 100)

(|*) :: Behavior Float -> Behavior Float -> Behavior Float
(|*) = lift2 (*)
(|/) :: Behavior Float -> Behavior Float -> Behavior Float
(|/) = lift2 (/)
(|+) :: Behavior Float -> Behavior Float -> Behavior Float
(|+) = lift2 (+)
(|-) :: Behavior Float -> Behavior Float -> Behavior Float
(|-) = lift2 (-)

bThickCircle :: Behavior Float -> Behavior Float -> Behavior Picture
bThickCircle = lift2 thickCircle

thickCircle80 :: Behavior Picture
thickCircle80 = bThickCircle (constant 80) (constant 5)
thickCircleZoom :: Behavior Picture
thickCircleZoom = timeTransform (behZoom thickCircle80) (time |* constant 2)

bBlank = constant Graphics.Gloss.Blank
bArc :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bArc = lift3 arc
arc80 :: Behavior Picture
arc80 = bArc (constant 0) (constant 90) (constant 80)
arcZoom :: Behavior Picture
arcZoom = timeTransform (behZoom arc80) (time |* constant 2)

bPolygon :: Behavior Path -> Behavior Picture
bPolygon = lift1 polygon

bThickArc :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bThickArc = lift4 thickArc
thickArc80 :: Behavior Picture
thickArc80 = bThickArc (constant 0) (constant 90) (constant 90) (constant 5)
thickArcZoom :: Behavior Picture
thickArcZoom = timeTransform (behZoom thickArc80) (time |* constant 5)

bText :: Behavior String -> Behavior Picture
bText = lift1 text
helloWorld :: Behavior Picture
helloWorld = bText (constant "Hello World")
helloWorldZoom :: Behavior Picture
helloWorldZoom = timeTransform (behZoom helloWorld) (time |* constant 2)

behSquare :: Behavior Float -> Behavior Picture
behSquare l = bLine (lift1 (\r -> [(0,0),(0,r),(r,r),(r,0),(0,0)]) l)
behSquare80 :: Behavior Picture
behSquare80 = behSquare (constant 80)
behSquareZoom :: Behavior Picture
behSquareZoom = timeTransform (behZoom behSquare80) (time |* constant 2)
imTired :: Behavior Picture -> Behavior Picture
imTired p = timeTransform (behZoom p) (time |* constant 2)
behFillPolygon :: Behavior Picture -> Behavior Picture
behFillPolygon (Behavior p) = Behavior b where b t = let (Line a, ab) = p t in (polygon a, behFillPolygon ab)
behSquareFill80 :: Behavior Picture
behSquareFill80 = behFillPolygon behSquare80
behSquareFillZoom :: Behavior Picture
behSquareFillZoom = imTired behSquareFill80

bLineLoop :: Behavior Path -> Behavior Picture
bLineLoop = lift1 lineLoop

bRectanglePath :: Behavior Float -> Behavior Float -> Behavior Path
bRectanglePath = lift2 rectanglePath
behRectangle :: Behavior Float -> Behavior Float -> Behavior Picture
behRectangle h l = bLineLoop (bRectanglePath h l)
behRectangle80 :: Behavior Picture
behRectangle80 = behRectangle (constant 80) (constant 40)
behRectangleZoom :: Behavior Picture
behRectangleZoom = imTired behRectangle80


behCircleFill :: Behavior Picture -> Behavior Picture
behCircleFill (Behavior p) = Behavior b where b t = let (Circle a, ab) = p t in (circleSolid a, behFillPolygon ab)
behCircleFill80 :: Behavior Picture
behCircleFill80 = behCircleFill behCircle80
behCircleFilledZoom = imTired behCircleFill80

occ :: Event a -> (Float, a)
occ (Event f a) = (f,a)
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e = Behavior c where c t = let (te, b') = occ e in if t < te then (at b t, b) else (at b' t, b')--is there a way to make this better
eventRectangle :: Event (Behavior Picture)
eventRectangle = Event 10 behRectangleZoom




data Animation = Square Float | Rectangle Float Float | NewCircle Float Float
    | Rhombus Float |Text String |Blank |Arc Float Float Float Float
    | Scale Float Float Animation | Translate Float Float Animation
    |Rotate Float Animation | Combine Animation Animation | Zoom Animation | SinWiggle Float Float Animation
    |CosWiggle Float Float Animation |Spin Animation |TimeTransform Animation String Float
    |Until Animation (Event Animation)

execute :: Animation -> Behavior Picture
execute ani = case ani of
    Square l -> behSquare (constant l)
    Main.Rectangle h l -> behRectangle (constant h) (constant l)
    NewCircle r t -> bThickCircle (constant r) (constant t)
    Rhombus l -> behRhombus (constant l)
    Main.Text s -> bText (constant s)
    Main.Blank -> bBlank
    Main.Arc start end r t -> bThickArc (constant start) (constant end) (constant r) (constant t)
    Main.Scale x y a -> bscale (constant x) (constant y) (execute a)
    Main.Translate x y a -> btranslate (constant x) (constant y) (execute a)
    Main.Rotate d a -> bRotate (constant d) (execute a)
    Combine a b -> combinePicture (execute a) (execute b)
    Zoom a -> behZoom (execute a)
    SinWiggle hi lo a -> behWiggleRange (constant hi) (constant lo) behWiggleMotion (execute a)
    CosWiggle hi lo a -> behWiggleRange (constant hi) (constant lo) behWiggleMotionCos (execute a)
    Spin a -> behSpin (execute a)
    TimeTransform a operator amount -> case operator of
        "+" -> timeTransform (execute a) (time |+ constant amount)
        "-" -> timeTransform (execute a) (time |- constant amount)
        "*" -> timeTransform (execute a) (time |* constant amount)
        "/" -> timeTransform (execute a) (time |/ constant amount)
    Until a (Event f e) -> untilB (execute a) (Event f (execute e))

spinningRhombus :: Animation
spinningRhombus = TimeTransform (Spin (Rhombus 80)) "*" 50
spinningSquare = TimeTransform (Spin (Square 80)) "*" 50
sinWiggle = SinWiggle 200 0 (NewCircle 80 1)
cosWiggle = CosWiggle 200 0 (NewCircle 80 1)
testTwoWiggle = Combine (Main.Translate (-100) 0 sinWiggle) (Main.Translate 100 0 cosWiggle)
testThreeWiggle = Combine testTwoWiggle (Main.Translate 300 0 sinWiggle)

testEvent = Until spinningRhombus (Event 5 spinningSquare)

(+=>) :: Event a -> (Float -> a -> b) -> Event b
(+=>) e f = Event te (f te x) where (te,x) = occ e
(==>) :: Event a -> (a -> b) -> Event b
(==>) ev g = ev +=> (\_ x -> g x)
(*=>) :: Event a -> (Float -> b) -> Event b
(*=>) ev h = ev +=> (\t _ -> h t)
(-=>) :: Event a -> b -> Event b
(-=>) ev x' = ev +=> (\_ _ -> x')

constEv :: Float -> a -> Event a
constEv = Event



