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
        usedBehavior <- newIORef behHelloWorldZoom--this is where I insert the behavior I want to run

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

--first variable in the tuple is the time when the event occurs
--second variable in the tuple is what acutally occurs at said time
--e.g. the behavior that occurs at time t
occ :: Event a -> (Float, a)
occ (Event t a) = (t,a)

--first parameter is the behavior that the behavior starts out as
--second parameter is the event where the behavior changes
--so at the saved time of the event, the first behavior changes into the behavior stored inside the event (second behavior)
--result is a behavior that does what is listed above
--e.g. first parameter is `a`, second parameter is `Event(t,b)`, so the resulting behavior starts out as `a` until `t` where it then becomes `b`
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e =  Behavior c where c t = let (te, b') = occ e in if t <= te then (at b t, Behavior c) else (at b' t, Behavior c)

time :: Behavior Float
time = Behavior (\t -> (t, time))--call on the current time inside the animation

--changes the speed of a behavior
--used by passing a parameter that is an "equation of time", e.g. (time|-5) to delay it by 5 seconds
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
wiggleWithRange hi lo wiggleFunc = lo + (hi-lo) * (wiggleFunc+1)/2

dt :: Float
dt = 0.01--delta time. How much is incremented in the recursive call when calculating integral and predicate


--bounce related


--used for bounce where bounce ensures the starting position isn't below minY if the given starting position is less than minY
--use (at min 0) because its a constant so it'll work, this isnt' the best way
--this was only made because for some reason it wouldn't compile unless in a seperate function, it said the typing was wrong or something
--if min is larger than starting position, it'll take min, otherwise starting position
startBounceY :: Behavior Float -> Float -> Float
startBounceY minY = max (at minY 0)




--wave behavior related

--adds a point to a wave based on time
--the function is e.g. either sin or cos
--note that w and h are intended to be used on sin and cos waves
addPoint :: Float -> Float -> Float -> (Float -> Float) -> Point
addPoint t w h f= (t*w, h*f t)

--aux function that is used to build a path for a wave
--acc is the resulting path
--has fixed with of 10 and height of 100
--time has increments of dt
waveAux :: [Point] -> Float -> (Float -> Float) -> Float -> Path
waveAux acc ti f t = if ti>=t then acc else waveAux (addPoint ti 10 100 f:acc) (ti+dt) f t










--functions that take behaviors as argument
----------------------------------------------------------------------------------------------------------------------------------------------
--gloss functions


bScale :: Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bScale = lift3 scale--change size of shape via x and y factors. e.g. used for zoom

bTranslate ::  Behavior Float -> Behavior Float -> Behavior Picture -> Behavior Picture
bTranslate = lift3 translate--moves shape from default, which is (0,0)

bRotate :: Behavior Float -> Behavior Picture -> Behavior Picture
bRotate = lift2 rotate--rotates a given picure by the given Behavior Float in degrees

--closed loop on a path, otherwise despite whatever points are in the path there will be a gap if normal line is used instead
--used for shapes with no fill
bLineLoop :: Behavior Path -> Behavior Picture
bLineLoop = lift1 lineLoop

--created a rectangle centered on the origin (0,0)
--first parameter is width and the second one is height
bRectanglePath :: Behavior Float -> Behavior Float -> Behavior Path
bRectanglePath = lift2 rectanglePath

--if the shape has no fill, the outline is colored, otherwise fill
--if e.g. two circles with no fill then both outlines are colored
--if a behavior that already has a color gets "colored again", it won't get colored and it'll stay the same color (see behColorPriorityTest for example)
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

(<=*) :: Behavior Float -> Behavior Float -> Behavior Bool
(<=*) = lift2 (<=)

(>=*) :: Behavior Float -> Behavior Float -> Behavior Bool
(>=*) = lift2 (>=)

(&&*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)

(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(||*) = lift2 (||)


--growing line


bAddPointToLine :: Behavior Point -> Behavior Path -> Behavior Path
bAddPointToLine = lift2 (:)

--a straight horizontal line that becomes longer as time passes
--the factor is how fast the line grows
bGrowingLineWithFactor :: Behavior Float -> Behavior Picture
bGrowingLineWithFactor = lift2 (\t f -> Line [(0,0), (f*t, 0)]) time



--bounce related stuff


--takes two behaviors and turns them into one behavior that contains the contents of both behaviors in a tuple
bPair :: Behavior a -> Behavior a -> Behavior (a,a)
bPair (Behavior a) (Behavior b) = Behavior c where c t = let (aa, ab) = a t; (ba, bb) = b t in  ((aa,ba), bPair ab bb)

bFloatToString :: Behavior Float -> Behavior String
bFloatToString (Behavior b) = Behavior c where c t = let (ab, bb) = b t in (show ab, bFloatToString bb)

--can create a vertical bounce behavior (no horizontal)
--min and max are min and max y values for the **center** of the circle, not for the edges of the circle
--start is starting y position
--v is starting velocity
--g is assumed to be gravity (-9.8)
--shape is assumed to be circle
bVerticalBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
bVerticalBounce minY maxY r v start = let properStart = startBounceY minY start in bTranslate (constant 0) (fBounce minY maxY properStart v (constant (-9.8)) 0) (bCircle r)

--same as bProperBounce but creates a square around the bounce behavior so the circle collides inside the square
bVerticalBounceWithSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
bVerticalBounceWithSquare minY maxY r v start= bCombinePictures (bVerticalBounce minY maxY r v start) (bBounceSquare minY maxY r)

--calculates the length sqaure that should be drawn around bounce using the min and max y values and the radius of the circle
--radius is used to calculate when the sides collide, as technically the min and max is for the center of the circle
--this only applies when the shape is a circle
bSquareLengthForBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float
bSquareLengthForBounce minY maxY r = (maxY |- minY) |+ (r |* constant 2)

--creates square for bounce and moves it into the correct place
--assumes bounce occurs in the middle (y=0)
--for vertical bounce
bBounceSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bBounceSquare minY maxY r = bTranslate (bSquareLengthForBounce minY maxY r |/ constant (-2)) (minY |- r) (bSquare (bSquareLengthForBounce minY maxY r))

--same as verticalBounceWithSquare but bounces both horizontally and vertically
--min and max for both x and y are assumed to be the same value
bAlllBounceWithSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
bAlllBounceWithSquare minAll maxAll r v start = bCombinePictures (bAllBounce minAll maxAll r v start) (bAllSquare minAll maxAll r)

--same as verticalBounceWithSquare but bounces both horizontally and vertically
--min and max for both x and y are assumed to be the same value
bAllBounce :: Behavior Float -> Behavior Float -> Behavior Float -> Float ->  Float -> Behavior Picture
bAllBounce minAll maxAll r v start = let properStart = startBounceY minAll start in bTranslate (fBounce minAll maxAll properStart v (constant 0) 0) (fBounce minAll maxAll properStart v (constant (-9.8)) 0) (bCircle r)

--same as bProperSquare but the bounce bounces both horizontally and vertically
--min and max for both x and y are assumed to be the same value
bAllSquare :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Picture
bAllSquare minAll maxAll r = bTranslate (constant (-1) |* minAll) (minAll |- r) (bSquare (bSquareLengthForBounce minAll maxAll r))




--wave behavior related


--for recursive function that keeps adds a circle 30 to the right
--counter is for the amount of circles that have been added
--all circles zoom and have a radius on 68
bCircleAux :: Float -> Behavior Picture
bCircleAux counter = bTranslate (constant (counter*30)) (constant 0) (bZoom (bCircle (constant 68)))

--creates 20 zooming circles that are 30 to the right of each other
--circles have radius 68
--acc is the behavior that contain all of these circles
--the circles dont zoom at the same time and have a different of 10 seconds in regards to timeTransform
--the more the circle is on the right the more "ahead" it is in its zoom
--all circles zoom at the same rate but are on different "stages" of the zoom, i.e. differnet points on the sin function
--counter is used to count the amount of circles, determine where the circle is placed and also determine the zoom of the circle
bManyCirclesAux :: Float -> Behavior Picture -> Behavior Picture
bManyCirclesAux counter acc = if counter==20 then acc else bManyCirclesAux (counter+1) (bCombinePictures acc (timeTransform (bCircleAux counter) (time |+ constant (counter+10))))
--(btranslate (constant (counter*10)) (constant 0) (behZoom (behCircle (constant 67))) (time |+ constant counter)))))






--forever circle related


bForeverAux :: Behavior Picture -> Float -> Float -> Behavior Picture
bForeverAux b t amount= if t==amount then b `untilB` eventACircleLater b t else bForeverAux (b `untilB` eventACircleLater b t) (t+1) amount

bBetterCircleLater :: Behavior Picture -> Float -> Behavior Picture
bBetterCircleLater b counter = bCombinePictures b (bTranslate (constant (counter*100)) (constant 0) behCircleZoom)






--other


--takes shape and zooms in and out
--the shape is largest when its original size
--zooms via zoomValue (abs sin)
--zooms by scaling x and y by equal factor
bZoom :: Behavior Picture -> Behavior Picture
bZoom = bScale (lift1 zoomValue time) (lift1 zoomValue time)

bFasterZoom :: Behavior Picture -> Behavior Picture
bFasterZoom p = timeTransform (bZoom p) (time |* constant 2)--speeds it up by 2

bSpin :: Behavior Picture -> Behavior Picture
bSpin = bRotate time--continuously rotates a given picture by constantly rotating with the time

bAbs :: Behavior Float -> Behavior Float
bAbs = lift1 abs



--please do not use these, these functions are either useless or dangerous

--meant to take a picture that is a shape (lineLoop) and turn it in to a polygon, and it works, but it is very easy prone to error
--since it accepts any picture, if given a picture that is not a lineLoop, it will just crash
--also this is more object-orientated
bFillPolygon :: Behavior Picture -> Behavior Picture
bFillPolygon (Behavior p) = Behavior b where b t = let (Line a, ab) = p t in (polygon a, bFillPolygon ab)

--takes circle behavior and fills it, but has same problem as bFillPolygon
--it does technically work though
--also this is more object-orientated
bCircleFill :: Behavior Picture -> Behavior Picture
bCircleFill (Behavior p) = Behavior b where b t = let (Circle a, ab) = p t in (circleSolid a, bCircleFill ab)













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



--bounce related stuff


--used in bounce to calculate the velocity and the position of the ball
--b is the behavior that represents the function of the integral
--t0 is the starting time from where the integral start
--note that the acc in fIntRec is 0 as it represents the area under the curve, as this function is executed when a new integral is created
--fIntRec is not given a `t` parameter so the contents is `Float -> (Float, Behavior Float)`
--`t` is also inserted by sample
fIntegral :: Behavior Float -> Float -> Behavior Float
fIntegral b t0 = Behavior (fIntRec b t0 0)

--used by integral to calculate the area under a curve
--area is calculated by calculating the area of a small rectangle where height is the value of Behavior b at time ti and the width is dt
--this area gets added to an accumulator that is the result
--runs until ti>=t, where ti is incremented by dt every recursion
--the reason not ti==t is because of float point arithmetic and also so they dont have to be the exact same value. Solves the problem of dt because kind of large
--returns a tuple to prevent memory leak. The tuple is for the content of the Behavior, as it can then be inserted like in fIntegral
--memory leak is prevented by storing the ti and acc by storing a `Float -> (Float, Behavior Float)` by storing fIntRec inside a behavior that sample can use instead to carry its calculation where it left off rather than start over from scratch
--`t` must be the last parameter, as it allows for a `Float -> (Float, Behavior Float)` function to be returned, as t is the value that the sample function will insert
fIntRec :: Behavior Float -> Float -> Float -> Float -> (Float, Behavior Float)
fIntRec (Behavior b) ti a t = if ti>=t then (a, Behavior (fIntRec (Behavior b) ti a)) else fIntRec b' (ti+dt) (a + a'*dt) t where (a',b')=b ti

--creates a behavior of the y position of a bouncing shape
--y0 is the starting position and v0 is the starting velocity and t0 is the starting time
fBounce :: Behavior Float -> Behavior Float -> Float -> Float -> Behavior Float -> Float -> Behavior Float
fBounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

        --used to basically have an infinite loop, as start is recursive and gets different values when collision happens
        --start is called whenever collision happens, which is represented by the event `doBounce`
        --(doBounce +=> start) creates an event that occurs when a collision occurs and contains `start t0 (y0, v0)`, where t0 is the time of collision and (y0, v0) are the values inside doBounce
        --basically start is defined as "perform this behavior until collision, where start is called again but with the values inside doBounce"
        --(y0, v0) inside doBounce is the y coordinate of the shape and the new lower velocity
        start t0 (y0,v0) = y `untilB` ( doBounce +=> start)

            where y = constant y0 |+ fIntegral v t0--integral of velocity is distance traveled
                  v = constant v0 |+ fIntegral g t0--behavior that contains the velocity and a given time, as velocity changes due to gravity/acceleration. Integral of acceleration is velocity
                  reciprocity = 0.8--velocity becomes 0.8 slower when collision

                  --firstly doBounce takes the value of y and v at the time collision happens
                  --then pipes it into snd so the () that comes from collide gets disregarded, resulting in an Event only containing (y,v)
                  --afterwards, v gets multiplied with reciprocity and y stays the same, and these values are the contents of the event doBounce
                  --reminder that ==> pipes the content of an Event into a function and returns an Event containing the result of that function
                  doBounce =  snapshot collide (bPair y v) ==> snd ==> (\ (yHit, vHit) ->
                               (yHit,- (reciprocity * vHit)))
                  --y can be <= minVal and v has to be 0 or a negative number (the shape must be heading downwards)
                  --y can also be >= maxVal and v has to be 0 or positive number (shape is heading upwards)
                  --again, <= and >= are used to avoid float point arithmetic and also allows dt to be larger
                  collide = predicate ((y <=* minVal &&* (v <=* constant 0)) ||*
                                        (y >=* maxVal &&* (v >=* constant 0))) t0

fLongerBounce :: Behavior Float -> Behavior Float -> Float -> Float -> Behavior Float -> Float -> Behavior Float
fLongerBounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

        start t0 (y0,v0) = y `untilB` ( doBounce +=> start)

            where y = constant y0 |+ fIntegral v t0
                  v = constant v0 |+ fIntegral g t0
                  reciprocity = 0.97--this is the only difference between this and fBounce

                  doBounce =  snapshot collide (bPair y v) ==> snd ==> (\ (yHit, vHit) ->
                               (yHit,- (reciprocity * vHit)))--also isnt run
                  collide = predicate ((y <=* minVal &&* (v <=* constant 0)) ||*
                                        (y >=* maxVal &&* (v >=* constant 0))) t0

--fLongerBounce where the parameters are filled out
--longerBounce when min is 0, max is 500, starts at 400 where v is 100 and gravity is -9.8
fUseLongerBounce :: Behavior Float
fUseLongerBounce = fLongerBounce (constant 0) (constant 500) 400 100 (constant (-9.8)) 0

--bounce where there is no acceleration nor decelleration as gravity isn't used at all
--therefore integrals are used at all
--velocity still changes but only when collision happens
--values are the same as fBounce
fSimplerBounce :: Float -> Float -> Float -> Float -> p -> Float -> Behavior Float
fSimplerBounce minVal maxVal y0 v0 g t0 = start t0 (y0, v0) where

                      start t0 (y0,v0) = y `untilB` (doBounce +=> start)

                          where y = constant y0 |+ (v |* bAbs (time |- constant t0))--starting position + (velocity * the amount of time that has passed since collision)
                                v = constant v0--am i supposed to sample here?
                                reciprocity = 0.8

                                doBounce = snapshot collide (bPair y v) ==> snd ==> \ (yHit, vHit) ->
                                                               (yHit,- (1 * (reciprocity * vHit)))
                                collide = predicate ((y <=* constant minVal &&* (v <=* constant 0)) ||*
                                                      (y >=* constant maxVal &&* (v >=* constant 0))) t0

--fBounce where the parameters are filled out
--min is 0, max is 100, starts at 90, v starts out as -50, gravity is -9.8
fUseBounce :: Behavior Float
fUseBounce = fBounce (constant 0) (constant 100) 90 (-50) (constant (-9.8)) 0

--fSimpleBounce where the parameters are filled out
--min is 0, max is 100, starts at 100, v starts out as 50, gravity is -9.8 (gravity isnt' used)
fUseSimpleBounce :: Behavior Float
fUseSimpleBounce = fSimplerBounce 0 100 100 50 (constant (-9.8)) 0




--wave behavior related


--recursive function that adds a point in a line that is supposed to look like a wave
--very similar to fIntRec
--acc is the line as a picture
--f is e.g. sin or cos
--w and h are the width and height of the line (as in the width of the wave in a sine curve and the height of the sin curve)
--note that w and h are intended to be used on sin and cos waves
--note that dt isn't used here, but instead 0.001 is used
--just like fIntRec this function returns a tuple to avoid memory leak by storing the point where the recursive function has reached to so sampling doesn't have to start from scratch
--this is the simplified behavior
--similarly, t has to be the last parameter so a `Float -> (Picture, Behavior Picture)` can be created inside a behavior
--`t` is also not stored inside the stored behavior because the behavior has to contain a function and so the stored behavior has a function that sample can utilize
fWaveRec :: [Point] -> Float -> Float -> Float -> (Float -> Float) -> Float -> (Picture, Behavior Picture)
fWaveRec acc ti w h f t =if ti>=t then (line acc, Behavior (fWaveRec acc ti w h f)) else fWaveRec (addPoint ti w h f:acc) (ti+0.001) w h f t

--a behavior that displays "t = *insert current time*" and changes with the time
sTimeText :: Behavior String
sTimeText =  Behavior b where b t = ("t = " ++ show t, sTimeText)--idk where else to put it






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

--for the square around bounce where the values of bounce is behBounce
--created to figure out what the values should be for the function
--square has length 200 because the center travels 100 and then the radius is 50, so 100+50+50=200
behSquareAround :: Behavior Picture
behSquareAround = bTranslate (constant (-50)) (constant (-50)) (bSquare (constant 200))




--wave behavior related


--this was created to see how the program would behave performance-wise if there are a lot of points
--basically behTimeTransformTestSinWave after a minute
behLargeSinWave :: Behavior Picture
behLargeSinWave = Behavior b where b _ = (line (waveAux [] 0 sin (60*5)), behLargeSinWave)

--also more for debugging (look at performance)
--there is a trace here
--100 circles 10 apart and each have radius of 80
behALotOfCircles :: Behavior Picture
behALotOfCircles = aux behCircle80 1 where aux acc counter = trace "aux" ( if counter==101 then acc else aux (bCombinePictures acc (bTranslate (constant (10*counter)) (constant 0) behCircle80)) (counter+1))






--please do not use these, these functions are either useless or dangerous
--they do technically work, but please dont


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





--event stuff


--this bool sees if sin (pi*t)==1
--used to see if zoom is at the largest part or if a wiggle is at the top
--the boolean is handled like this to ensure no floating point arithmetic error
boolSaved :: Behavior Bool
boolSaved = lift1 (\t -> abs(sin (t*pi)-1) < 1e-10) time

--predicate test that changes the wiggle to be of a circle to a square the first time the wiggle reaches the top after 5 seconds
behPredicate :: Behavior Picture
behPredicate = behWiggle `untilB` eventPredicate

--boolean that checks if t is divisible by 5
--used if you want something to be true/happen every 5 seconds
--need to use round because `t` is a float so otherwise this will never be true
boolForCircle :: Behavior Bool
boolForCircle = lift1 (\t -> round t `mod` 5==0) time--idk where else to put it







--bounce related stuff


--bounce that takes longer to finish
--longerBounce when min is 0, max is 500, starts at 400 where v is 100 and gravity is -9.8
--reciprocity is 0.97
--its a circle and the radius is 50
--this is just vertical
behLongerBounce :: Behavior Picture
behLongerBounce = bTranslate (constant 0) fUseLongerBounce (bCircle (constant 50))

--mainly used this for testing
--min is 0, max is 100, radius of circle is 50, gravity is -9.8, reciprocity is 0.8., starting point is 90 and starting v is -50
behBounce :: Behavior Picture
behBounce = bTranslate (constant 0) fUseBounce (bCircle (constant 50))

--bounce with no acceleration/gravity
--circle with radius 50
--min is 0, max is 100, starts at 100, v starts out as 50, gravity is -9.8 (gravity isnt' used)
behSimplerBounce :: Behavior Picture
behSimplerBounce = bTranslate (constant 0) fUseSimpleBounce (bCircle (constant 50))


--displays what the y value is supposed to be for the bounce
--must be between 0-100
--uses the values of behBounce/fUseBounce
behBounceFloat :: Behavior Picture
behBounceFloat = bText (bFloatToString fUseBounce)

--behBounce with a square around
--the circle is not in the middle of the square, but instead leaning towards the left side (so its touching the left side)
behBounceSquare :: Behavior Picture
behBounceSquare = bCombinePictures behSquareAround behBounce

--vertical bounce with square around where shape is in middle
--circle with 100 radius
--min is 50, max is 200, starting v is 70 and starting position is 0
behVerticalBounce :: Behavior Picture
behVerticalBounce = bVerticalBounceWithSquare (constant 50) (constant 200) (constant 100) 70 0

--bounce in all directions with square around where shape is in middle
--circle with 100 radius
--min is 50, max is 200, starting v is 70 and starting position is 0
--there is slight lag when collision happen at the current moment
behAllBounce :: Behavior Picture
behAllBounce = bAlllBounceWithSquare (constant 50) (constant 200) (constant 100) 70 0

--same as behBounceSquare but the circle is actually in the middle of the square
--compared to behAllBounce there is no lag
behBetterBounceSquare :: Behavior Picture
behBetterBounceSquare = bVerticalBounceWithSquare (constant 0) (constant 100) (constant 50) 50 100

--a bounce in all directions where the shape is a small ball
--since the ball is small, it bounces in a much more varied way
--no square
--the min is 0, max is 300, v starts at 40, starting position is 42
--circle has radius 10
behSmallBall :: Behavior Picture
behSmallBall = bAllBounce (constant 0) (constant 300) (constant 10) 40 42


--wave behavior related


--a growing sin wave of width 10 and height 100
--slow if you are inpatient but you can see the growth
--note this doesn't use dt
behSinWave :: Behavior Picture
behSinWave = Behavior b where b= fWaveRec [] 0 10 100 sin

--behSinWave but 5 times faster
--mainly tested stuff on this
--note this doesn't use dt
behTimeTransformTestSinWave :: Behavior Picture
behTimeTransformTestSinWave = timeTransform behSinWave (time |* constant 5)

--there is a trace
--100 circles that are 10 apart that zoom as one picture
--for debugging in regards to performance
behMassiveCircleZoom :: Behavior Picture
behMassiveCircleZoom = bZoom behALotOfCircles

--creates 19 zooming circles that are 30 to the right of each other
--circles have radius 68
--the circles dont zoom at the same time and have a different of 10 seconds in regards to timeTransform
--all circles zoom at the same rate but are on different "stages" of the zoom, i.e. differnet points on the sin function
--the circle at the origin has a red outline
behManyCirclesIrregularZoom :: Behavior Picture
behManyCirclesIrregularZoom = bManyCirclesAux 1 (bZoom (bColor (constant red) (bCircle (constant 68))))

--displays what the time is at the current moment (so how many seconds have passed)
--in the format of "t = *insert time*"
behTimeText :: Behavior Picture
behTimeText = bText sTimeText




--forever circle related


behHomeeee :: Behavior Picture
behHomeeee = behCircleZoom `untilB` eventACircleLater behCircleZoom 1

behAgain :: Behavior Picture
behAgain = behHomeeee `untilB` eventACircleLater behHomeeee 2

behForever :: Behavior Picture
behForever = bForeverAux behCircleZoom 1 20








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
behRectangleZoom = bFasterZoom behRectangle--length is 80 and height is 40

behColor :: Behavior Picture
behColor = bColor (constant red) behTwoWiggle--outline of the two circles in wiggle become bright red

--two circles with red outline wiggle and in the middle there is a zooming circle that has a violet outline
--all have a radius of 80
--also shows that because the zooming circle was already colored purple when the entire picture got colored red the zooming circle stays violet
behColor2 :: Behavior Picture
behColor2 = bColor (constant red) (behTwoWiggle `bCombinePictures` bColor (constant violet) behCircleZoom)

--it displays the exact same as behColor, but it demostrates how bColor works
--if a picture has already been colored and it gets colored again, the color wont change at it stays as the first color
behColorPriorityTest :: Behavior Picture
behColorPriorityTest = bColor (constant violet) behColor--this is red, i.e. the exact same as behColor








--please do not use these, these functions are either useless or dangerous

behSquareFillZoom :: Behavior Picture
behSquareFillZoom = bFasterZoom behSquareFill80--square is filled and its side has length 80

behCircleFilledZoom :: Behavior Picture
behCircleFilledZoom = bFasterZoom behCircleFill80--circle has radius 80





--Functions that return an event
----------------------------------------------------------------------------------------------------------------------------------------------
constEv :: Float -> a -> Event a
constEv = Event--takes time and the value inside event e.g. behavior

--predicate contains time when the given boolean is true (that why it returns a Event (), because its only purpose is the time
--second parameter is from when the predicate is valid (so basically from which point in time should predicate start checking from
--figures out the time by recursively checking from t and then increments t by dt until it finds a point in time where the boolean is true
--if never true then its an infinite loop
predicate :: Behavior Bool -> Float -> Event ()
predicate (Behavior b) t = Event te () where te = let aux acc = if fst (b acc) then acc else aux (acc+dt) in aux t

--returns an event containing both the value of the event and the value of a behavior at the point in time where the event occurs
--so if event ´a´ occurs at time `t`, then it checks what value behavior `b` had at `t`
--the event returned also still has the time of when the given event occured
snapshot :: Event a -> Behavior b -> Event (a,b)
snapshot e (Behavior b) = Event te (x, fst (b te)) where (te,x) = occ e

--if there is an event inside an event, it makes it to a normal event that can be used
--used for event sequencing and for when the operators below are used, as sometimes it can create a result that would need a joinEv to "fix"
joinEv :: Event (Event a) -> Event a
joinEv e = snd (occ e)


--saved events


eventRectangle :: Event (Behavior Picture)
eventRectangle = Event 10 behRectangleZoom--at time 10 the behavior is behRectangleZoom

--the first time (sin t*pi)==1 (max value) after 5 seconds the behavior becomes behWiggleSquare
eventPredicate :: Event (Behavior Picture)
eventPredicate = predicate boolSaved 5 -=> behWiggleSquare






--forever circle related


eventACircleLater :: Behavior Picture -> Float -> Event (Behavior Picture)
eventACircleLater b counter = predicate boolForCircle ((counter-1)*5+1) -=> bCombinePictures b (bTranslate (constant (counter*100)) (constant 0) behCircleZoom)


--operators


--piping but with events
--takes an event and applies a function on the values inside the event (time and the value) and returns an event
(+=>) :: Event a -> (Float -> a -> b) -> Event b
(+=>) e f =  Event te (f te x) where (te,x) = occ e

--same as (+=>) but the function doesnt take the time value
(==>) :: Event a -> (a -> b) -> Event b
(==>) ev g = ev +=> (\_ x -> g x)

--same as (+=>) but only time gets taken as a parameter
(*=>) :: Event a -> (Float -> b) -> Event b
(*=>) ev h = ev +=> (\t _ -> h t)

--used for building an event with a predicate
--creates an event that occurs at the same time as first parameter but contains the second parameter
(-=>) :: Event a -> b -> Event b
(-=>) ev x' = ev +=> (\_ _ -> x')

--takes whichever of the events occurs first
(.|.) :: Event a -> Event a -> Event a
(.|.) e e' = if te<=t'e then Event te x else Event t'e x' where (te,x)=occ e; (t'e,x')=occ e'





--debugging stuff
----------------------------------------------------------------------------------------------------------------------------------------------
printPredicateEvent :: (Float, Behavior Picture)
printPredicateEvent = occ eventPredicate--i just wanted to print out the tuple of the predicateEvent




--bounce related


--intRec that doesn't handle memory leak and only returns the accumulated integral
--not even a behavior
--behavior does not get simplified
fBadIntRec :: Behavior Float -> Float -> Float -> Float -> Float
fBadIntRec (Behavior b) ti a t  = if ti>= t then a else fBadIntRec b' (ti+dt) (a + a'*dt) t where (a',b')=b ti

--behavior that doesn't take account memory leaks by not simplifying behaviors
data Behavior2 a = Behavior2 (Float -> a)

--like fBadIntRec but with Behavior2
fBadIntRec2 :: Behavior2 Float -> Float -> Float -> Float -> Float
fBadIntRec2 (Behavior2 b) ti a t  = if ti>= t then a else fBadIntRec2 (Behavior2 b) (ti+dt) (a + a'*dt) t where a' = b ti

printInc :: Float
printInc = fst (fIntRec (constant 9.8) 0.0001 0 0.0005)--just a print to see how intRec would run if dt was small

--integral with badIntRec
--uses Behavior, not Behavior2
fBadIntegral :: Behavior Float -> Float -> Behavior Float
fBadIntegral b t0 = Behavior b1 where b1 t = (fBadIntRec b t0 0 t, fBadIntegral b t0)

--integral with badIntRec2, so Behavior 2
fIntegral2 :: Behavior2 Float -> Float -> Behavior2 Float
fIntegral2 b t0 = Behavior2 (fBadIntRec2 b t0 0)





--IO stuff


wrongIntRec :: Behavior Float -> Float -> Float -> Float -> IO (Float , Behavior Float)
wrongIntRec (Behavior b) ti a t = do
    --x <- readIORef recCounter      --used when measuring amount of recursion
    --writeIORef recCounter (x+1)    --used to count recursion
    tracking ti t a               --used to track ti and t and also print other stuff, basically anything other than recCounter
    --increment                    -- used to increment recCounter at some point
    atomicWriteIORef prevAcc a     --write the current acc and since it happens after the tracking function where acc is checked, it tracks the "previous" acc
    atomicWriteIORef prevT0 ti     --same as acc but with t0/ti
    if ti>=t then let c = fIntRec (Behavior b) ti a in return (a, Behavior c) else wrongIntRec b' (((ti+dt)*100)/100) (a + a'*dt) t where (a',b')=b ti--this is just regular intRec. Tried fixing some floating point arithmetic thingy but it did nothing

prevT0 :: IORef Float
prevT0 = unsafePerformIO $ newIORef 0--global variable of previous t0

prevAcc :: IORef Float
prevAcc = unsafePerformIO $ newIORef 0--global variable of previous acc

wrongIntegral :: Behavior Float -> Float -> IO (Behavior Float)
wrongIntegral b t0 = trace (show t0) (do
        writeIORef recCounter 0--restarts recCounter as there is a new integral. recCounter is for checking how many recursive calls between two collisions
        let d = Behavior c where c t = unsafePerformIO (wrongIntRec b t0 0 t)--behaves the same as integral but changes intRec to be not IO
        return d)

--only prints something if t0 is exactly 0 (no rounding)
--shouldn't be zero at any point in the future except the beginning
checkT0 :: Float -> IO ()
checkT0 t0 = do if abs(t0-0) < 1e-10 then print "its zero" else return ()

{-# NOINLINE recCounter #-}
recCounter :: IORef Integer
recCounter = unsafePerformIO $ newIORef 0--global variable for how many recursive calls between two collisions/in one integral. for checking if there is a memory leak

increment :: IO ()
increment = do x <- readIORef recCounter
               if (x `mod` 50000 == 0) then whenTrue else writeIORef recCounter (x+1)--increments recCounter regardless, but also prints out recCounter every 50000 calls

--prints out recCounter and then increments it
whenTrue :: IO ()
whenTrue = do x <- readIORef recCounter
              print x
              writeIORef recCounter (x+1)

--note that whatever value of ti, t and a is used is from whatever is passed into the function as a parameter, not from a global variable
tracking :: Float -> Float -> Float -> IO ()
tracking ti t a = do
                     --if ti>1 then trackingTrue ti t else return ()            prints out t and ti when ti>1
                     --(abs(ti-0)< 1e-10)           --the boolean for above but handles floating point arithemtic
                     x <- readIORef prevT0
                     --y <- readIORef prevAcc

                     --supposed to measure when ti becomes smaller compared to the previous value, which should never happen. then prints out ti, t, recCounter and acc if this ever occurs
                     --note that prevAcc is permanently 0 as it wasn't relevent in the way i thought it was supposed to be
                     --doesn't take floating point arithimic into account
                     if ti<x then print ("ti: " ++ show ti ++ " x: " ++ show x ++ " acc: " ++ show a ++ " prevacc: " ++ show 0) else return ()

                     --checks if t<1 and if ti==0
                     --only the ti computation takes into account floating point arithimic
                     --if true then prints out ti, t, recCounter and acc
                     --if (t<1&&(abs(ti-0)< 1e-10)) then print ("t0: " ++ show ti ++ " " ++ "t: " ++ show t ++ " counter: " ++ show x ++ " acc: " ++ show a) else return ()

--prints out ti and t. ti and t come from the parameters of the function
trackingTrue :: Float -> Float -> IO ()
trackingTrue ti t = do print ("ti: " ++ show ti)
                       print ("t: " ++ show t)

--changes currentT to the t that is passed into the function as a parameter and then prints out that t that is passed into the function
tChange :: Integer -> IO ()
tChange t = do writeIORef currentT t
               print t

currentT :: IORef Integer
currentT = unsafePerformIO $ newIORef 0



--sample debugging


--used for testing sample in regards to memory leak
--behavior contains t until t>=5 where it then contains 0 and the simplified behavior is t-5
--basically the behavior at t>=5 starts all over and then continues on "as normal" but 5 seconds delayed forever 
simpleBeh :: Behavior Float
simpleBeh = Behavior b1
   where
   b2 t =(t - 5, Behavior b2)
   b1 t | t < 5 = (t, Behavior b1)
        | otherwise = (0, Behavior b2)

sampleTest :: Behavior Picture
sampleTest = bTranslate (simpleBeh|*constant 100) (constant 0) behCircle80

simpleBeh2 :: Behavior Float
simpleBeh2 = Behavior b1 where b1 t = (t, Behavior b1)

predicateTest5 :: Behavior Bool
predicateTest5 = lift1 (>= 5) time

untilBTestPredicate :: Event (Behavior Float)
untilBTestPredicate = predicate predicateTest5 0 -=> timeTransform simpleBeh2 (time |- constant 5)

sampleTest2 :: Behavior Float
sampleTest2 = simpleBeh `untilB` untilBTestPredicate

sampleTest2Exe :: Behavior Picture
sampleTest2Exe = bTranslate (sampleTest2|*constant 100) (constant 0) behCircle80





--code I used beforehand
--intRec _ t0 a _ | trace ("intRec t0:" ++  " " ++ show t0) False = undefined
--integral (Behavior b) t0 | trace ("integral second behavior : " ++ show (at (snd (b t0)) t0)) False = undefined
--integral _ t0 | trace ("integral t0: " ++ show t0) False = undefined
--tracking ti = do x <- readIORef tiCounter
{-do x <- readIORef counter
                       if x `mod` 1000==0 then print (show x) else -}







--rest of stuff
----------------------------------------------------------------------------------------------------------------------------------------------

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




--bounce :: Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Behavior Float -> Float -> Behavior Float



--distanceTraveled (Behavior v) = Behavior b where b t =  (fst (v t) * t, distanceTraveled (Behavior v))



--bounce :: Behavior Float -> Behavior Float



--sinWave :: Float -> Float -> Float -> Path
--pointAux :: Float -> Float -> [Point] -> [Point]
--pointAux t ti acc = if ti>=t then acc else pointAux t (ti+1) ((addPoint t):acc)

--sinWave = sinAux [] 0

--sinAux _ _ _ | trace "sinAux called" False = undefined
--sinAux acc ti t = trace ("sinAux called") (if ti>=t then acc else sinAux (addPoint ti 10 100:acc) (ti+dt) t)



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


--forever :: Behavior Picture
--foreverAux :: Behavior Picture -> Float -> (Picture, Behavior Picture)





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

