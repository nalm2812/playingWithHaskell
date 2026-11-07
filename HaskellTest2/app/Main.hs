{- HLINT ignore "Use newtype instead of data" -}
module Main(main) where

import Graphics.Gloss
import Debug.Trace
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Interface.IO.Animate
import GHC.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (atomicWriteIORef)


main :: IO ()


main = do
        usedBehavior <- newIORef behColor--this is where I insert the behavior I want to run

        --sample function that turns the Behavior into a (Float -> Picture) that the animateIO function can use
        --this sample function also makes sure to use the simplest behavior that is given (the behavior inside the tuple)
        let sample t = atomicModifyIORef' usedBehavior (\beh -> let extract (Behavior b) = b t; use = extract beh in (snd use, fst use))
        --gloss' animate function, which means gloss takes care of the visual and how frequencently sampling happens
        --controllerSetRedraw indicates that every time sample is executed the picture will be redrawn
        --first tuple in window is for the size of the window, the second tuple is the position of the window on your own screen in regards to the top left corner I think???
        animateIO (InWindow "Nice Window" (1000, 1000) (0, 0)) white sample controllerSetRedraw

--data types
----------------------------------------------------------------------------------------------------------------------------------------------
data Behavior a = Behavior (Float -> (a, Behavior a))
data Event a = Event Float a

--standard functions when working with behaviors and events
----------------------------------------------------------------------------------------------------------------------------------------------
--lift functions
--lift functions "lifts" a regular functiona and makes it a function that accepts behehaviors
--e.g. a function with type "Float -> Float" becomes "Behavior Float -> Behavior Float"
--this function also handles simplifying behaviors

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f (Behavior fa) = Behavior fb where fb t = let(a,ba) = fa t in (f a, lift1 f ba)

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f (Behavior fa) (Behavior fb) = Behavior fc where fc t =  let(a,ba) = fa t; (b,bb) = fb t in (f a b, lift2 f ba bb)

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 f (Behavior fa) (Behavior fb) (Behavior fc) = Behavior fd where fd t = let(a,ba) = fa t; (b,bb) = fb t; (c, bc) = fc t in (f a b c, lift3 f ba bb bc)

lift4 :: (a -> b -> c -> d -> e) -> Behavior a -> Behavior b -> Behavior c -> Behavior d -> Behavior e
lift4 f (Behavior fa) (Behavior fb) (Behavior fc) (Behavior fd) = Behavior fe where fe t = let(a,ba) = fa t; (b,bb) = fb t; (c, bc) = fc t; (d,bd) = fd t in (f a b c d, lift4 f ba bb bc bd)


constant :: a -> Behavior a
constant a = Behavior ab where ab _ = (a, constant a)--creates a behavior thats the same regardless of time

at :: Behavior a -> (Float -> a)
at (Behavior b) t = fst (b t)--produces the value of Behavior `a` at Time `t`

time :: Behavior Float
time = Behavior (\t -> (t, time))--call on the current time inside the animation

timeTransform :: Behavior a -> Behavior Float -> Behavior a
timeTransform (Behavior b) (Behavior t2) = Behavior c where c t = let (ta, tb) = t2 t; (a, ab) = b ta in (a, timeTransform ab tb)


--non-behavior functions
----------------------------------------------------------------------------------------------------------------------------------------------
circle80 :: Picture
circle80 = Circle 80--circle with radius 80

zoomValue :: Float -> Float
zoomValue number =abs (sin number)--translate time to the size a shape should be in regards to zoom

atSinWave :: Float -> Picture
atSinWave = at behSinWave--tests if `at` returns the correct function that can be used in animate (not animateIO)

--wiggle functions

wiggleMotion :: Float -> Float
wiggleMotion t = sin (pi*t)--translate time to wiggle position

cosWiggleMotion :: Float -> Float
cosWiggleMotion t = cos (pi*t)--time to wiggle position with cos

--same as wiggle but can decide min and max height
--the wiggle parameter allows it to take a "wiggle function", used to determine whether the wiggle follows sin or cos or if the function is changed in any other way (like speed)
wiggleWithRange :: Float -> Float -> Float -> Float
wiggleWithRange hi lo wiggleMotion = lo + (hi-lo) * (wiggleMotion+1)/2




--functions that take behaviors as argument
----------------------------------------------------------------------------------------------------------------------------------------------
--gloss functions


bScale :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bScale = lift3 scale--change size of shape via x and y factors. e.g. used for zoom

bTranslate ::  Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bTranslate = lift3 translate--moves shape from default, which is (0,0)

bRotate :: Behavior Float -> Behavior Picture -> Behavior Picture
bRotate = lift2 rotate--rotates a given picure by the given Behavior Float in degrees

bLineLoop :: Behavior Path -> Behavior Picture
bLineLoop = lift1 lineLoop

bRectanglePath :: Behavior Float -> Behavior Float -> Behavior Path
bRectanglePath = lift2 rectanglePath

bColor :: Behavior Color -> Behavior Picture -> Behavior Picture
bColor = lift2 color





--shapes


bCircle :: Behavior Float -> Behavior Picture
bCircle = lift1 Circle--takes radius of circle as parameter

bRhombus :: Behavior Float -> Behavior Picture
bRhombus l = bLine (lift1 (\r -> [(-r,0),(0,r),(r,0),(0,-r),(-r,0)]) l)--l is length of the sides of the rhombus

bLine :: Behavior Path -> Behavior Picture
bLine = lift1 line--takes collection of points and creates a picture of a line

bThickCircle :: Behavior Float -> Behavior Float -> Behavior Picture
bThickCircle = lift2 thickCircle--line thickness

--first two variable is the angle in degress where the arc is in between and the last variable is radius
--example of angle is an arc between 0 and 90 degrees
bArc :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bArc = lift3 arc

bThickArc :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bThickArc = lift4 thickArc--last parameter is line thickness

bPolygon :: Behavior Path -> Behavior Picture
bPolygon = lift1 polygon--takes path and make it a polygon by filling it. Probably also closes

bText :: Behavior String -> Behavior Picture
bText = lift1 text

bSquare :: Behavior Float -> Behavior Picture
bSquare l = bLineLoop (lift1 (\r -> [(0,0),(0,r),(r,r),(r,0),(0,0)]) l)--l is lenght of side

bRectangle :: Behavior Float -> Behavior Float -> Behavior Picture
bRectangle l h = bLineLoop (bRectanglePath l h)





--wiggle-related functions

--takes shape and moves it in the y-axis by wiggleRange
bTranslatedWiggleRange :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bTranslatedWiggleRange hi lo wiggleFunction= bTranslate (constant 0) (fWiggleWithRange hi lo wiggleFunction)

--creates wiggle behavior where the user can decide where on the x-axis the behavior occurs
--does same as bTranslatedWiggleRange but with more stuff
--minHeight is always 0
--shape is always a circle of radius 80
bWiggleMoveLocation :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bWiggleMoveLocation x wiggle hi = bTranslate x (constant 0) (bTranslatedWiggleRange hi (constant 0) wiggle behCircle80)

--same as bWiggleMoveLocation (as it is also called) but the wiggle motion is already determined to be using a sin curve
--specifically, the wiggle motion is fWiggleMotion
--can still determine x position and max height
bSinWiggleMoveLocation :: Behavior Float -> Behavior Float -> Behavior Picture
bSinWiggleMoveLocation x = bWiggleMoveLocation x fWiggleMotion

--same as above but with cos
bCosWiggleMoveLocation :: Behavior Float -> Behavior Float -> Behavior Picture
bCosWiggleMoveLocation x = bWiggleMoveLocation x fWiggleMotionCos


--combining pictures functions

--turns a Behavior [Picture] into a Behavior Picture
--basically turns a collection of pictures into a single picture that can be executed
bCollectionToPicture :: Behavior [Picture] -> Behavior Picture
bCollectionToPicture = lift1 pictures

--combines two pictures into one picture that can be executed
bCombinePictures :: Behavior Picture -> Behavior Picture -> Behavior Picture
bCombinePictures a b = bCollectionToPicture (a |: (b |: behEmptyPictureCollection))



--operators

--adds a Behavior picture to a Behavior of a collection of pictures and returns Behavior of a collection
--note that it does not return a `Behavior Picture` that can be executed
(|:) :: Behavior Picture -> Behavior [Picture] -> Behavior [Picture]
(|:) = lift2 (:)


--growing line


bAddPointToLine :: Behavior Point -> Behavior Path -> Behavior Path
bAddPointToLine = lift2 (:)

--a straight horizontal line that becomes longer as time passes
--the factor is how fast the line grows
bGrowingLineWithFactor :: Behavior Float -> Behavior Picture
bGrowingLineWithFactor = lift2 (\t f -> Line [(0,0), (f*t, 0)]) time




--other


--takes shape and zooms in and out
--the shape is largest when its original size
--zooms via zoomValue (abs sin)
--zooms by scaling x and y by equal factor
bZoom :: Behavior Picture -> Behavior Picture
bZoom = bScale (lift1 zoomValue time) (lift1 zoomValue time)

bDoubleZoom :: Behavior Picture -> Behavior Picture
bDoubleZoom p = timeTransform (bZoom p) (time |* constant 2)--speeds it up by 2

bSpin :: Behavior Picture -> Behavior Picture
bSpin = bRotate time--continuously rotates a given picture by constantly rotating with the time



--please do not use these, these functions are either useless or dangerous

bFillPolygon :: Behavior Picture -> Behavior Picture
bFillPolygon (Behavior p) = Behavior b where b t = let (Line a, ab) = p t in (polygon a, bFillPolygon ab)

bCircleFill :: Behavior Picture -> Behavior Picture
bCircleFill (Behavior p) = Behavior b where b t = let (Circle a, ab) = p t in (circleSolid a, bFillPolygon ab)




--Behavior Float functions that are arguments for other functions
--These floats change value depending on the time
----------------------------------------------------------------------------------------------------------------------------------------------
--wiggle-related functions


--takes wiggle function and also adds time, so the value of wiggle depends on time
--used to only define "the shape of the wiggle" (as in either sin or cos wave)
fWiggleMotion :: Behavior Float
fWiggleMotion = lift1 wiggleMotion time

--same as above but with cos
fWiggleMotionCos :: Behavior Float
fWiggleMotionCos = lift1 cosWiggleMotion time

--creates a behavior that receives a wiggleMotion and then creates a wiggle behavior with a max height and min height
--this is the one that gets used by translate, fWiggleMotion isn't used for translate at all
fWiggleWithRange :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float
fWiggleWithRange  = lift3 wiggleWithRange





--operators

--these can e.g. be used for timeTransform
(|*) :: Behavior Float -> Behavior Float -> Behavior Float
(|*) = lift2 (*)

(|/) :: Behavior Float -> Behavior Float -> Behavior Float
(|/) = lift2 (/)

(|+) :: Behavior Float -> Behavior Float -> Behavior Float
(|+) = lift2 (+)

(|-) :: Behavior Float -> Behavior Float -> Behavior Float
(|-) = lift2 (-)



--standard behavior constants that I often use
----------------------------------------------------------------------------------------------------------------------------------------------

behCircle80 :: Behavior Picture
behCircle80 = constant circle80

behEmptyPictureCollection :: Behavior [Picture]
behEmptyPictureCollection = constant []--used to start a collection of pictures

behRhombus50 :: Behavior Picture
behRhombus50 = bRhombus (constant 50)--length of sides are 50

behThickCircle80 :: Behavior Picture
behThickCircle80 = bThickCircle (constant 80) (constant 5)--thickness is 5

behBlank :: Behavior Picture
behBlank = constant Graphics.Gloss.Blank--literally nothing

behArc80 :: Behavior Picture
behArc80 = bArc (constant 0) (constant 90) (constant 80)--angle is between 0 and 90 degrees

behThickArc90 :: Behavior Picture
behThickArc90 = bThickArc (constant 0) (constant 90) (constant 90) (constant 5)--angle is between 0 and 90 degrees with thickness of 5

behHelloWorld :: Behavior Picture
behHelloWorld = bText (constant "Hello World")

behSquare80 :: Behavior Picture
behSquare80 = bSquare (constant 80)--side is length 80

behRectangle :: Behavior Picture
behRectangle = bRectangle (constant 80) (constant 40)--length 80, height 40





--please do not use these, these functions are either useless or dangerous


behSquareFill80 :: Behavior Picture
behSquareFill80 = bFillPolygon behSquare80--square with fill that has length 80 side

behCircleFill80 :: Behavior Picture
behCircleFill80 = bCircleFill behCircle80--radius 80



--Behavior Picture
--These are executable or are specific tests
----------------------------------------------------------------------------------------------------------------------------------------------
--wiggle-related tests

--circle with radius 80 wiggles using a sin curve motion
--min is 0 and max is 100
--the behavior occurs in the middle of the window (0,0)
behWiggle :: Behavior Picture
behWiggle = bTranslatedWiggleRange (constant 100) (constant 0) fWiggleMotion behCircle80

--same as behWiggle but with a square instead
--square lengths are 80
behWiggleSquare :: Behavior Picture
behWiggleSquare = bTranslatedWiggleRange (constant 100) (constant 0) fWiggleMotion behSquare80

--two circles of radius 80 wiggles. The wiggles are different due to different wiggle motions
--left wiggle uses a sin curve and x=-100
--right wiggle uses cos curve and x=100
--both wiggles have max of 100
behTwoWiggle :: Behavior Picture
behTwoWiggle = bCollectionToPicture (behFirstWiggle |: (behSecondWiggle |: behEmptyPictureCollection))

behFirstWiggle :: Behavior Picture
behFirstWiggle = bSinWiggleMoveLocation (constant (-100)) (constant 100)

behSecondWiggle :: Behavior Picture
behSecondWiggle = bCosWiggleMoveLocation (constant 100) (constant 100)

--others


behCircleZoom :: Behavior Picture
behCircleZoom = bZoom behCircle80--circle of 80 radius zooms

--zoom is sped up by a factor of 5
--length of side is 50
behRhombusZoom :: Behavior Picture
behRhombusZoom = timeTransform (bZoom behRhombus50) (time |* constant 5)

--length of side is 50
--extremely slow
behRhombuSpin :: Behavior Picture
behRhombuSpin = bSpin (bRhombus (constant 50))

--same as above but faster
behRhombusSpinFast :: Behavior Picture
behRhombusSpinFast = timeTransform behRhombuSpin (time |* constant 100)

behGrowingLine :: Behavior Picture
behGrowingLine = bGrowingLineWithFactor (constant 10)--grows with factor of 10

--thick circle has radius 80 and thickness of 5
--zoom is sped up by 2
behThickCircleZoom :: Behavior Picture
behThickCircleZoom = timeTransform (bZoom behThickCircle80) (time |* constant 2)

--arc is between angle 0 and 90 and has radius 80
--zoom sped up by 2
behArcZoom :: Behavior Picture
behArcZoom = timeTransform (bZoom behArc80) (time |* constant 2)

--arc is between angle 0 and 90 and has radius 90
--line thickness of 5
--zoom sped up by 5
behThickArcZoom :: Behavior Picture
behThickArcZoom = timeTransform (bZoom behThickArc90) (time |* constant 5)

behHelloWorldZoom :: Behavior Picture
behHelloWorldZoom = timeTransform (bZoom behHelloWorld) (time |* constant 2)--zoom sped up by 2

--side have length 80
--zoom sped up by 2
behSquareZoom :: Behavior Picture
behSquareZoom = timeTransform (bZoom behSquare80) (time |* constant 2)

behRectangleZoom :: Behavior Picture
behRectangleZoom = bDoubleZoom behRectangle--length is 80 and height is 40

behColor :: Behavior Picture
behColor = bColor (constant red) behTwoWiggle--outline of the two circles in wiggle become bright red





--please do not use these, these functions are either useless or dangerous

behSquareFillZoom :: Behavior Picture
behSquareFillZoom = bDoubleZoom behSquareFill80--square is filled and its side has length 80

behCircleFilledZoom :: Behavior Picture
behCircleFilledZoom = bDoubleZoom behCircleFill80--circle has radius 80















































occ :: Event a -> (Float, a)
occ (Event f a) = (f,a)
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e =  Behavior c where c t = let (te, b') = occ e in if t <= te then (at b t, Behavior c) else (at b' t, Behavior c)
eventRectangle :: Event (Behavior Picture)
eventRectangle = Event 10 behRectangleZoom

(+=>) :: Event a -> (Float -> a -> b) -> Event b
(+=>) e f =  Event te (f te x) where (te,x) = occ e
(==>) :: Event a -> (a -> b) -> Event b
(==>) ev g = ev +=> (\_ x -> g x)
(*=>) :: Event a -> (Float -> b) -> Event b
(*=>) ev h = ev +=> (\t _ -> h t)
(-=>) :: Event a -> b -> Event b
(-=>) ev x' = ev +=> (\_ _ -> x')

constEv :: Float -> a -> Event a
constEv = Event

predicate :: Behavior Bool -> Float -> Event ()
predicate (Behavior b) t = Event te () where te = let aux acc = if fst (b acc) then acc else aux (acc+dt) in aux t--predicate contains time when its true
behBool :: Behavior Bool
behBool = lift1 (\t -> abs((sin (t*pi))-1) < 1e-10) time
testPredicate :: Behavior Picture
predicateEvent = predicate behBool 5 -=> behWiggleSquare
testPrint = occ predicateEvent
testPredicate = untilB behWiggle predicateEvent


(.|.) :: Event a -> Event a -> Event a
(.|.) e e' = if te<=t'e then Event te x else Event t'e x' where (te,x)=occ e; (t'e,x')=occ e'

snapshot :: Event a -> Behavior b -> Event (a,b)
snapshot e (Behavior b) = Event te (x, fst (b te)) where (te,x) = occ e

joinEv :: Event (Event a) -> Event a
joinEv e = snd (occ e)

--data VectorSpace a = Vector
--integral t t0 = Integral t t0
--behIntegral :: VectorSpace a => Behavior a -> Behavior Float -> Behavior a
dt :: Float
dt = 0.01--when one it does move but very slowly and not smooth
wrongIntRec :: Behavior Float -> Float -> Float -> Float -> IO (Float , Behavior Float)
--intRec _ t0 a _ | trace ("intRec t0:" ++  " " ++ show t0) False = undefined
prevT0 = unsafePerformIO $ newIORef 0
prevAcc = unsafePerformIO $ newIORef 0
wrongIntRec (Behavior b) ti a t = do
    --x <- readIORef counter
    --writeIORef counter (x+1)
    tracking ti t a
    --increment
    atomicWriteIORef prevAcc a
    atomicWriteIORef prevT0 ti
    if ti>=t then let c t = (intRec (Behavior b) ti a t) in return (a, Behavior c) else wrongIntRec b' (((ti+dt)*100)/100) (a + a'*dt) t where (a',b')=b ti

badIntRec :: Behavior Float -> Float -> Float -> Float -> Float
badIntRec (Behavior b) ti a t  = if ti>= t then a else badIntRec b' (ti+dt) (a + a'*dt) t where (a',b')=b ti
data Behavior2 a = Behavior2 (Float -> a)
badIntRec2 :: Behavior2 Float -> Float -> Float -> Float -> Float
badIntRec2 (Behavior2 b) ti a t  = if ti>= t then a else badIntRec2 (Behavior2 b) (ti+dt) (a + a'*dt) t where a' = b ti
--incTest = fst (intRec (constant 9.8) 0.0001 0 0.0005)
wrongIntegral :: Behavior Float -> Float -> IO (Behavior Float)
--integral (Behavior b) t0 | trace ("integral second behavior : " ++ show (at (snd (b t0)) t0)) False = undefined
--integral _ t0 | trace ("integral t0: " ++ show t0) False = undefined
wrongIntegral b t0 = trace (show t0) (do
        writeIORef counter 0
        let d = Behavior c where c t = unsafePerformIO (wrongIntRec b t0 0 t)
        return d)
checkT0 :: Float -> IO ()
checkT0 t0 = do if abs(t0-0) < 1e-10 then print "its zero" else return ()
integral b t0 = Behavior (intRec b t0 0)
intRec (Behavior b) ti a t = if ti>=t then (a, Behavior (intRec (Behavior b) ti a)) else intRec b' (ti+dt) (a + a'*dt) t where (a',b')=b ti

badIntegral b t0 = Behavior b1 where b1 t = (badIntRec b t0 0 t, badIntegral b t0)
integral2 b t0 = Behavior2 (badIntRec2 b t0 0)
--Behavior c where c t = (badIntRec b t0 0 t, integral b t0)
--Behavior (intRec b t0 0)
--integral b a f =
--    sum [
--        f t * 0.0001 |
--        t <- [a + 0.0001 / 2, a + 3*0.0001/2 .. b - 0.0001 /2]
--        ]
--recursive function that does this instead
--start with t0 (Float), then sample (get a value and new behavior),
--the new behavior is basically the recursive function so it can be further called if you want to further sample later on


--behIntegral :: Behavior Float -> Float -> Behavior Float
--behIntegral f t0 = lift4 integral time t0 (constant (at f)) (constant 0.0001)--this is definetly wrong


(<=*) :: Behavior Float -> Behavior Float -> Behavior Bool
(<=*) = lift2 (<=)
(>=*) :: Behavior Float -> Behavior Float -> Behavior Bool
(>=*) = lift2 (>=)
(&&*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)
(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(||*) = lift2 (||)
pairB :: Behavior a -> Behavior a -> Behavior (a,a)
pairB (Behavior a) (Behavior b) = Behavior c where c t = let (aa, ab) = a t; (ba, bb) = b t in  ((aa,ba), pairB ab bb)
--bounce :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Float -> Behavior Float

{-# NOINLINE counter #-}

counter = unsafePerformIO $ newIORef 0
increment = do x <- readIORef counter
               if (x `mod` 50000 == 0) then whenTrue else writeIORef counter (x+1)
whenTrue = do x <- readIORef counter
              print x
              writeIORef counter (x+1)
tracking :: Float -> Float -> Float -> IO ()
tracking ti t a = do
                     --if ti>1 then trackingTrue ti t else return ()
                     --(abs(ti-0)< 1e-10)
                     x <- readIORef prevT0
                     --y <- readIORef prevAcc
                     if ti<x then print ("ti: " ++ show ti ++ " x: " ++ show x ++ " acc: " ++ show a ++ " prevacc: " ++ show 0) else return ()
                     --if (t<1&&(abs(ti-0)< 1e-10)) then print ("t0: " ++ show ti ++ " " ++ "t: " ++ show t ++ " counter: " ++ show x ++ " acc: " ++ show a) else return ()
trackingTrue :: Float -> Float -> IO ()
trackingTrue ti t = do print ("ti: " ++ show ti)
                       print ("t: " ++ show t)
tChange t = do writeIORef currentT t
               print t
--tracking ti = do x <- readIORef tiCounter


currentT = unsafePerformIO $ newIORef 0





{-do x <- readIORef counter
                       if x `mod` 1000==0 then print (show x) else -}

--bounce :: Behavior Float -> Behavior Float -> Float -> Float -> Behavior Float -> Float -> Behavior Float
bounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

        start t0 (y0,v0) =
                     y `untilB` ( doBounce +=> start)--start and untilB are called less

            where y = constant y0 |+ (integral v t0)--calculate the behavior picture inside here
                  v = constant v0 |+ (integral g t0)--seperate integral function so prints are easier to read, figure out what ti is in integral
                  reciprocity = 0.8

                  doBounce =  snapshot collide (pairB y v) ==> snd ==> (\ (yHit, vHit) ->
                               (yHit,- (reciprocity * vHit)))--also isnt run
                  collide = predicate ((y <=* minVal &&* (v <=* constant 0)) ||*
                                        (y >=* maxVal &&* (v >=* constant 0))) t0
longerBounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

        start t0 (y0,v0) =
                     y `untilB` ( doBounce +=> start)--start and untilB are called less

            where y = constant y0 |+ (integral v t0)--calculate the behavior picture inside here
                  v = constant v0 |+ (integral g t0)--seperate integral function so prints are easier to read, figure out what ti is in integral
                  reciprocity = 0.97

                  doBounce =  snapshot collide (pairB y v) ==> snd ==> (\ (yHit, vHit) ->
                               (yHit,- (reciprocity * vHit)))--also isnt run
                  collide = predicate ((y <=* minVal &&* (v <=* constant 0)) ||*
                                        (y >=* maxVal &&* (v >=* constant 0))) t0

useLongerBounce = longerBounce (constant 0) (constant 500) 400 100 (constant (-9.8)) 0
behLongerBounce = bTranslate (constant 0) useLongerBounce (bCircle (constant 50))


--distanceTraveled (Behavior v) = Behavior b where b t =  (fst (v t) * t, distanceTraveled (Behavior v))
behAbs = lift1 abs
simplerBounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

                      start t0 (y0,v0) = y `untilB` (doBounce +=> start)--as if it calls start right here

                          where y = constant y0 |+ (v |* behAbs (time |- constant t0))--calculate the behavior picture inside here
                                v = constant v0--am i supposed to sample here?
                                reciprocity = 0.8

                                doBounce = snapshot collide (pairB y v) ==> snd ==> \ (yHit, vHit) ->
                                                               (yHit,- (1 * (reciprocity * vHit)))--how is the time noted here? whats with the 0.2
                                collide = predicate ((y <=* constant minVal &&* (v <=* constant 0)) ||*
                                                      (y >=* constant maxVal &&* (v >=* constant 0))) t0

changeBounce a b reciprocity = (a, constant (-1) |* constant reciprocity |* b)
noSnapshot minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

                                 start t0 (y0,v0) = y `untilB` (doBounce +=> start)

                                     where y = y0 |+ (v |* behAbs (time |- constant t0))--calculate the behavior picture inside here
                                           v = v0
                                           reciprocity = 0.8

                                           doBounce = collide -=> changeBounce y v reciprocity
                                           collide = predicate ((y <=* minVal &&* (v<=*constant 0)) ||*
                                                                 (y >=* maxVal &&* (v>=*constant 0))) t0

behBounce :: Behavior Picture
useBounce :: Behavior Float
useBounce = bounce (constant 0) (constant 100) 90 (-50) (constant (-9.8)) 0
behBounce = bTranslate (constant 0) useBounce (bCircle (constant 50))
useBounceSimple = simplerBounce 0 100 100 50 (constant (-9.8)) 0
behSimplerBounce = bTranslate (constant 0) useBounceSimple (bCircle (constant 50))

noSnapshotBounce = noSnapshot (constant 0) (constant 100) (constant 100) (constant 50) (constant (-9.8)) 0
behNoSnapshot = bTranslate (constant 0) noSnapshotBounce (bCircle (constant 50))

bFloatToString :: Behavior Float -> Behavior String
bFloatToString (Behavior b) = Behavior c where c t = let (ab, bb) = b t in (show ab, bFloatToString bb)


behBounceFloat = bText (bFloatToString useBounce)



squareAround :: Behavior Picture
squareAround = bTranslate (constant (-50)) (constant (-50)) (bSquare (constant 200))
behBounceSquare :: Behavior Picture
behBounceSquare = bCombinePictures squareAround behBounce
properBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
iGiveUp :: Behavior Float -> Float
iGiveUp min = at min 0
uhhhh min = max (iGiveUp min)
properBounce min max r v start = let properStart = uhhhh min start in bTranslate (constant 0) (bounce min max properStart v (constant (-9.8)) 0) (bCircle r)
properBounceWithSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
properBounceWithSquare min max r v start= bCombinePictures (properBounce min max r v start) (properSquare min max r)
squareLengthForBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float
squareLengthForBounce min max r = (max |- min) |+ (r |* constant 2)
properSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
properSquare min max r = bTranslate (squareLengthForBounce min max r |/ constant (-2)) (min |- r) (bSquare (squareLengthForBounce min max r))
testProperBounce :: Behavior Picture
testProperBounce = properBounceWithSquare (constant 50) (constant 200) (constant 100) 70 0


horizontalBounceWithSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
horizontalBounceWithSquare min max r v start = bCombinePictures (horizontalBounce min max r v start) (horizontalSquare min max r)
horizontalBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
horizontalBounce min max r v start = let properStart = uhhhh min start in bTranslate (bounce min max properStart v (constant 0) 0) (bounce min max properStart v (constant (-9.8)) 0) (bCircle r)
testHorizontalBounce :: Behavior Picture
testHorizontalBounce = horizontalBounceWithSquare (constant 50) (constant 200) (constant 100) 70 0
horizontalSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
horizontalSquare min max r = bTranslate (constant (-1) |* min) (min |- r) (bSquare (squareLengthForBounce min max r))
noLagBounce :: Behavior Picture
noLagBounce = properBounceWithSquare (constant 0) (constant 100) (constant 50) 50 100

smallBall :: Behavior Picture
smallBall = horizontalBounce (constant 0) (constant 300) (constant 10) 40 42


--bounce :: Behavior Float -> Behavior Float


addPoint :: Float -> Float -> Float -> (Float -> Float) -> Point
addPoint t w h f= (t*w, h*f t)
--sinWave :: Float -> Float -> Float -> Path
--pointAux :: Float -> Float -> [Point] -> [Point]
--pointAux t ti acc = if ti>=t then acc else pointAux t (ti+1) ((addPoint t):acc)
waveRec :: [Point] -> Float -> Float -> Float -> (Float -> Float) -> Float -> (Picture, Behavior Picture)
waveRec acc ti w h f t =if ti>=t then (line acc, Behavior (waveRec acc ti w h f)) else waveRec (addPoint ti w h f:acc) (ti+0.001) w h f t
--sinWave = sinAux [] 0
behSinWave :: Behavior Picture
behSinWave = Behavior b where b= waveRec [] 0 10 100 sin
timeTransformTestSinWave :: Behavior Picture
timeTransformTestSinWave = timeTransform behSinWave (time |* constant 5)

waveAux :: [Point] -> Float -> (Float -> Float) -> Float -> Path
--sinAux _ _ _ | trace "sinAux called" False = undefined
--sinAux acc ti t = trace ("sinAux called") (if ti>=t then acc else sinAux (addPoint ti 10 100:acc) (ti+dt) t)
waveAux acc ti f t = if ti>=t then acc else waveAux (addPoint ti 10 100 f:acc) (ti+dt) f t
largeSinWave :: Behavior Picture
largeSinWave = Behavior b where b _ = (line (waveAux [] 0 sin (60*5)), largeSinWave)

aLotOfCircles :: Behavior Picture
aLotOfCircles = aux behCircle80 1 where aux acc counter = trace "aux" ( if counter==101 then acc else aux (bCombinePictures acc (bTranslate (constant (10*counter)) (constant 0) behCircle80)) (counter+1))
massiveCircleZoom :: Behavior Picture
massiveCircleZoom = bZoom aLotOfCircles

timeText :: Behavior String
timeText =  Behavior b where b t = ("t = " ++ show t, timeText)
behTimeText :: Behavior Picture
behTimeText = bText timeText

circleAux counter = bTranslate (constant (counter*30)) (constant 0) (bZoom (bCircle (constant 67)))

manyCirclesAux :: Float -> Behavior Picture -> Behavior Picture
manyCirclesAux counter acc = if counter==20 then acc else manyCirclesAux (counter+1) (bCombinePictures acc (timeTransform (circleAux counter) (time |+ constant (counter+10))))
--(btranslate (constant (counter*10)) (constant 0) (behZoom (behCircle (constant 67))) (time |+ constant counter)))))

manyCirclesIrregularZoom :: Behavior Picture
manyCirclesIrregularZoom = manyCirclesAux 1 (bZoom (bColor (constant red) (bCircle (constant 67))))

boolForCircle :: Behavior Bool
boolForCircle = lift1 (\t -> round t `mod` 5==0) time
--problem comes from when you run start again its still true, so its forever true
--relative time instead of absolute time



--zoomNoBeh :: Float -> Picture

--addCircle :: Float -> Behavior Picture
--addCircle c = aux behCircleZoom 1 where aux acc counter = if counter==c then acc else aux (combinePicture acc behCircleZoom) (counter+1)

--iWantSleep :: Float -> Behavior Picture
--iWantSleep c = Behavior b where b t = aux behCircleZoom 1 where aux acc counter = if counter==c then (acc, Behavior (aux acc)) else aux
--cries = Behavior b where b t = ((at (addCircle t) t), addCircle)
--addingCircleEvent :: Behavior Picture
--addingCircleEvent =  predicate boolForCircle 0 -=> cries

--addCircle :: Picture -> Picture -> Picture
--addCircle c acc = pictures [c,acc]
--zoomNoBeh :: Float -> Picture
--zoomNoBeh t = scale value value (Circle 70) where value = abs (5*sin (t/5 ))
--ugh :: Picture -> Float -> Float -> (Float -> Picture, Behavior Picture)
--ugh acc counter t= if t==counter then (acc, Behavior (ugh acc counter)) else ugh (addCircle
aCircleLater :: Behavior Picture -> Float -> Event (Behavior Picture)
aCircleLater b counter = predicate boolForCircle ((counter-1)*5+1) -=> bCombinePictures b (bTranslate (constant (counter*100)) (constant 0) behCircleZoom)
homeeee :: Behavior Picture
homeeee = behCircleZoom `untilB` aCircleLater behCircleZoom 1
again :: Behavior Picture
again = homeeee `untilB` aCircleLater homeeee 2
--forever :: Behavior Picture
--foreverAux :: Behavior Picture -> Float -> (Picture, Behavior Picture)
foreverAux :: Behavior Picture -> Float -> Float -> Behavior Picture
foreverAux b t amount= if t==amount then b `untilB` aCircleLater b t else foreverAux (b `untilB` aCircleLater b t) (t+1) amount
testForever :: Behavior Picture
testForever = foreverAux behCircleZoom 1 20

betterCircleLater :: Behavior Picture -> Float -> Behavior Picture
betterCircleLater b counter = bCombinePictures b (bTranslate (constant (counter*100)) (constant 0) behCircleZoom)

--forever :: Float -> Behavior (Behavior Picture) -> Behavior Picture
{-forever t0 b = start t0 b where
        start t0 b = b `untilB` (ughhh +=> start)
            where ughhh = predicateTired `snapshot` b ==> snd ==> (\beh -> betterCircleLater beh (t0/5)) where predicateTired = predicate boolForCircle t0-}
            --where ughhh =
            --predicateTired -=> betterCircleLater b (t0/5) where predicateTired = predicate boolForCircle t0

--canIGoHome :: Behavior Picture
--canIGoHome = forever 0 behCircleZoom

{-addCircle :: Picture -> [Picture] -> [Picture]
addCircle p acc = p:acc
idkAnymoreAux :: Behavior Picture -> (Picture -> Float) -> Float -> Float -> (Picture, Behavior Picture)
idkAnymoreAux (Behavior b) acc counter t = if counter==t then (acc t, Behavior (idkAnymoreAux (Behavior b) acc counter)) else let (a', b') = b counter in idkAnymoreAux b'  -}



simpleBeh = Behavior b1
   where
   b2 t =(t - 5, Behavior b2)
   b1 t | t < 5 = (t, Behavior b1)
        | otherwise = (0, Behavior b2)



sampleTest :: Behavior Picture
sampleTest = bTranslate (simpleBeh|*constant 100) (constant 0) behCircle80

simpleBeh2 = Behavior b1 where b1 t = (t, Behavior b1)
predicateTest5 = lift1 (>= 5) time
untilBTestPredicate = predicate predicateTest5 0 -=> timeTransform simpleBeh2 (time |- constant 5)
sampleTest2 = simpleBeh `untilB` untilBTestPredicate
sampleTest2Exe = bTranslate (sampleTest2|*constant 100) (constant 0) behCircle80




















--behPentagon :: Behavior Float -> Behavior Picture
--behPentagon l = bLineLoop (lift1 (\r -> [(0,0),(0,r),(r,r),(r,0),(0,0)]) l)
