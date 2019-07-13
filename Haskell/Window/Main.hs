{- |
   Copyright   : Nah
   License     : Nah
   Maintainer  : Jack Ellis
   Stability   : nah
   Portability : portable
-}

module Main where

import Bindings
import Display
import Type
import Graphics.UI.GLUT
import Data.IORef

-- |What 'main' does is set up the window, create the "global variables" 'angle', 'delta',
--  and 'pos', and sets up the various callback functions, resulting in a call to 'mainLoop'
main :: IO ()
main = do (_progName, _args) <- getArgsAndInitialize
          initialWindowSize $= Size 640 480
          _window <- createWindow "Hello World"
          reshapeCallback $= Just reshape
          env <- newIORef $ Env {angle = 0.0, delta = 0.1, x = 0.0, y = 0.0}
          keyboardMouseCallback $= Just (keyboardMouse env)
          idleCallback $= Just (idle env)
          displayCallback $= (display env)
          mainLoop
