import Graphics.UI.GLUT
import System.Random
--import Data.IORef

data Dir = North | West | South | East deriving (Eq, Show)

data Env = Env {player :: Mover,
                enemies :: [Mover],
                time :: Int}

data Mover =  Frogger {
                x   :: Float
              , y   :: Float
              , vel :: Float
              , dir :: Dir
              }
           |  Car {
                x       :: Float
              , y       :: Float
              , vel     :: Float
              , len     :: Int
              }
           deriving Show

testF = Frogger {x = 0, y = 0, vel = 1, dir = North}
testC = Car {x = 0, y = 0, vel = 1, len = 3}

moverUpdate :: Mover -> Mover
moverUpdate f@(Frogger {x=fx, y=fy, vel=fvel, dir=fdir}) = case fdir of North -> f {y = fy + fvel}
                                                                        West -> f {x = fx + fvel}
                                                                        South -> f {y = fy - fvel}
                                                                        East -> f {x = fx - fvel}
moverUpdate c@(Car {x=cx, y=cy, vel=cvel}) = c {x = cx + cvel}

main' :: IO()
main' = do t <- get elapsedTime
           putStrLn $ show (div t 1000)
           main'


main :: IO()
main = do (_progName, args) <- getArgsAndInitialize
          main'

randomGen :: IO Mover
randomGen = (randomIO :: IO Float) >>= \r -> if      0 <= r && r < 0.2   then return $ carGen 1
                                             else if 0.2 <= r && r < 0.4 then return $ carGen 2
                                             else if 0.4 <= r && r < 0.6 then return $ carGen 3
                                             else if 0.6 <= r && r < 0.8 then return $ carGen 4
                                             else                             return $ carGen 5

carGen :: Int -> Mover
carGen 1 = Car {x = 0.0,    y = 64.0,  vel = 1.2,  len = 1}
carGen 2 = Car {x = 480.0,  y = 128.0, vel = -1.2, len = 1}
carGen 3 = Car {x = 0.0,    y = 192.0, vel = 1.2,  len = 1}
carGen 4 = Car {x = 480.0,  y = 256.0, vel = -1.2, len = 1}
carGen 5 = Car {x = 0.0,    y = 320.0, vel = 1.2,  len = 1}
