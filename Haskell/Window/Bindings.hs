{- |
   Copyright   : Nah
   License     : Nah
   Maintainer  : Jack Ellis
   Stability   : nah
   Portability : portable
-}

module Bindings (idle, display, reshape, keyboardMouse) where

import Type
import Display
import Graphics.UI.GLUT
import Data.IORef

-- |'reshape' informs the window of what to do when the window size is altered by a user,
--  by dragging the corner of a window. Currently it just stretches the content of the window.
reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)
                  postRedisplay Nothing

-- |'keyboardMouse' deals with keyboard and mouse input; if the space key is pressed it reverses
--  the direction of the circle, up and down increase and decrease the speed of rotation respectively,
--  and WASD move the whole thing around.
keyboardMouse :: IORef Env -> KeyboardMouseCallback
keyboardMouse e key Down _ _ = case key of (Char ' ')           -> e $~! \env -> env {delta = (negate . delta) env}
                                           (SpecialKey KeyUp)   -> e $~! \env -> env {delta = (delta env)+0.1}
                                           (SpecialKey KeyDown) -> e $~! \env -> env {delta = (delta env)-0.1}
                                           (Char 'w')           -> e $~! \env -> env {y = (y env)+0.1}
                                           (Char 'a')           -> e $~! \env -> env {x = (x env)-0.1}
                                           (Char 's')           -> e $~! \env -> env {y = (y env)-0.1}
                                           (Char 'd')           -> e $~! \env -> env {x = (x env)+0.1}
                                           _                    -> return ()
keyboardMouse _ _ _ _ _ =                                          return ()
