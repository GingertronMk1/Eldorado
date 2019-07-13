module Type (Env(..)) where

data Env = Env { -- |The angle subtended by the top square
                 angle :: Float,
                 -- |The amount 'angle' should change by per tick
                 delta :: Float,
                 -- |The position of the centre in x
                 x     :: Float,
                 -- |The position of the centre in y
                 y     :: Float
               } deriving Show
