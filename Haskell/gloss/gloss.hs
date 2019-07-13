import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment

data Game = G {pX   :: Float
              ,pY   :: Float
              ,pdX  :: Float
              ,pdY  :: Float
              ,cX   :: Float
              ,cY   :: Float
              ,n    :: Float
              ,move :: Bool
              ,rGen :: StdGen
              }

xSize :: Float
xSize = 800
ySize :: Float
ySize = 600
ticks :: Int
ticks = 60

initGame :: Game
initGame = G {pX = 0
             ,pY = 0
             ,pdX = 0
             ,pdY = 0
             ,cX = xSize / 4
             ,cY = 0
             ,n = 0
             ,move = False
             ,rGen = mkStdGen . round $ xSize * ySize
             }

drawGame :: Game -> Picture
drawGame g = Pictures [color black $ translate (pX g) (pY g) $ circlePoints 5.0
                      ,translate (cX g) (cY g) $ circlePoints 10.0
                      ,translate (cX g) (cY g) $ Text $ show $ move g
                      ,translate 0 (-10) . scale 0.1 0.1 . Text . show . fst . next $ rGen g
                      ,translate 0 10 . scale 0.1 0.1 . Text . show . snd . next $ rGen g
                      ]
             where circlePoints sc = let p = 10
                                      in Polygon [(sc * (cos (2 * pi * n/p)), sc * (sin (2 * pi * n/p))) | n <- [1..p]]

inputHandle :: Event -> Game -> Game
inputHandle (EventKey (Char 'w') Down _ _) g@(G {pY = y})              = g {pY = y + 10}
inputHandle (EventKey (Char 'a') Down _ _) g@(G {pX = x})              = g {pX = x - 10}
inputHandle (EventKey (Char 's') Down _ _) g@(G {pY = y})              = g {pY = y - 10}
inputHandle (EventKey (Char 'd') Down _ _) g@(G {pX = x})              = g {pX = x + 10}
inputHandle (EventKey (SpecialKey KeySpace) Down _ _) g@(G {move = m}) = g {move = not m}
inputHandle (EventKey (SpecialKey KeyEsc) _ _ _) g                     = g
inputHandle _ g = g

fromRange :: Fractional a => (a, a) -> (a, a) -> a -> a
fromRange (lr, ur) (lt, ut) n = (n * (ut - lt) / (ur - lr)) + lt

updateGame :: Float -> Game -> Game
updateGame _ g@(G {pX = x, pY = y, pdX = pdx, pdY = pdy}) = let n' = round $ (n g) * (fromIntegral ticks)
                                                                ticks2 = ticks ^ 2
                                                                ticks' = fromIntegral $ mod n' ticks2
                                                                tickProp = fromRange (0.0, fromIntegral ticks2) (0.0, 1.0) ticks' :: Float
                                                                rGen' = snd . next $ rGen g
                                                             in g {pX = x + pdx
                                                                  ,pY = y + pdy
                                                                  ,cX = if move g == True then (xSize / 2.5) * (cos $ 2 * pi * tickProp)
                                                                                          else cX g
                                                                  ,cY = if move g == True then (ySize / 2.5) * (sin $ 2 * pi * tickProp)
                                                                                          else cY g
                                                                  ,n = if move g == True then (n g) + 1
                                                                                         else n g
                                                                  ,rGen = rGen'
                                                                  }

main :: IO ()
main = do argc <- getArgs
          putStrLn $ show argc
          play
            FullScreen --(InWindow "Frogger" (800,600) (0,0)) -- :: Display                     Display mode.
            white       -- :: Color                       Background colour.
            ticks       -- :: Int                         Number of simulation steps to take for each second of real time.
            initGame    -- :: world                       The initial world
            drawGame    -- :: (world -> Picture)          A function to convert the world a picture.
            inputHandle -- :: (Event -> world -> world)   A function to handle input events.
            updateGame  -- :: (Float -> world -> world)   A function to step the world one iteration. It is passed the period of time (in seconds) to be advanced.
