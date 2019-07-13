{- |
   Copyright   : Nah
   License     : Nah
   Maintainer  : Jack Ellis
   Stability   : nah
   Portability : portable
-}

module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Util
import Type

-- |'display' takes care of all of the actual drawing to the screen
display :: IORef Env ->  DisplayCallback
display e = do clear [ColorBuffer]
               loadIdentity
               env <- get e
               putStrLn $ show env
               scale (20/64) (20/48) (1 :: GLfloat)
               translate (Vector3 (x env) (y env) 0)
               rotate (angle env) (Vector3 0 0 1)
               forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do color (Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2))
                                                                    translate (Vector3 x y z)
                                                                    cube 0.1
               swapBuffers

-- |'idle' deals with updating the IORefs when the user isn't doing anything
idle :: IORef Env -> IdleCallback
idle e = do --env <- get e
            e $~! (\x -> x {angle = angle x + delta x})
            postRedisplay Nothing
