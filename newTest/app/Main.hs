module Main(main) where

import Graphics.UI.GLUT

main :: IO ()
--main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Arc 0 90 80)
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

