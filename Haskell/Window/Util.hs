module Util where

import Graphics.Rendering.OpenGL

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = let n' = fromIntegral n 
           in [(sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n']]
