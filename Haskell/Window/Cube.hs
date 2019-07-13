{- |
   Module      : Main
   Copyright   : Nah
   License     : Nah
   Maintainer  : Jack Ellis
   Stability   : nah
   Portability : portable
-}

module Cube where

import Graphics.UI.GLUT

-- |This converts a 3-tuple of points into a vertex so GLUT can deal with it nicely.
--  It's mostly because it's an ugly function that I'd like to abstract away somewhere.
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

-- |'cube' takes an argument 'w' and creates a cube about the origin of edge length 'w'
cube :: GLfloat -> IO ()
cube w = do renderPrimitive Quads $ do mapM_ vertex3f [( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
                                                       ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
                                                       ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
                                                       (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
                                                       ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
                                                       ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w)]
