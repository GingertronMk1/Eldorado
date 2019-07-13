module Util where

import Graphics.Rendering.OpenGL

type Mover = (Char, Int, Int, Int) -- (Kind, x, y, length)

type Frogger = (Int, Int, Int) -- (x, y, Lives)

type Env = (Frogger, [Mover]) -- The player, and all the obstacles

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = let n' = fromIntegral n 
           in [(sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n']]
