-- First, imports -----------------------------------------------------------------------------------------------------

import Data.List

-- Next, basic information --------------------------------------------------------------------------------------------

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

limit :: Int
limit = 5

data Player = X | B | O deriving (Eq, Ord)

instance Show Player where
  show X = "X"
  show B = "."
  show O = "O"

type Row = [Player]
type Col = Row

type Board = [Row]

-- Some useful helpers ------------------------------------------------------------------------------------------------

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

showBoard' :: Board -> String
showBoard' = (++ "\n" ++ replicate cols '-' ++ "\n" ++ (flatten . map show) [0..cols-1]) . flatten . intersperse "\n" . map (flatten . map show)

showBoard :: Board -> IO ()
showBoard = putStrLn . showBoard'

toInt :: String -> Int
toInt "0" = 0
toInt "1" = 1
toInt "2" = 2
toInt "3" = 3
toInt "4" = 4
toInt "5" = 5
toInt "6" = 6
toInt "7" = 7
toInt "8" = 8
toInt "9" = 9
toInt _   = 0

numPieces = length . filter (/=B) . flatten

whoseGo :: Board -> Player
whoseGo b = if mod (numPieces b) 2 == 0 then X else O

-- Actual game stuff --------------------------------------------------------------------------------------------------

emptyBoard :: Board
emptyBoard = (replicate rows . replicate cols) B

diagonals :: Board -> [Row]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:(diagonals (map tail xss)))

allDiagonals :: Board -> [Row]
allDiagonals board = (diagonals board) ++ (diagonals $ map reverse board)

makeMove :: Int -> Player -> Board -> Board
makeMove n p b = transpose (colsBefore ++ [changedCol] ++ colsAfter)
                 where newBoard = transpose b
                       colsBefore = take (n) newBoard
                       colsAfter = drop (n+1) newBoard
                       changedCol = changeCol p (newBoard !! n)

fillCol :: Col -> Col
fillCol bs = if length bs < rows then fillCol (B:bs) else bs

changeCol :: Player -> Col -> Col
changeCol p c = fillCol (p:dropWhile (==B) c)

isFull :: Board -> Bool
isFull = null . filter (==B) . flatten

-- Has someone won? ---------------------------------------------------------------------------------------------------

xHas4 :: Row -> Bool
xHas4 = elem (replicate win X) . group

oHas4 :: Row -> Bool
oHas4 = elem (replicate win O) . group

has4 :: Row -> Player
has4 r
  | xHas4 r   = X
  | oHas4 r   = O
  | otherwise = B

whoWon :: Board -> Player
whoWon b = if players /= [] then head players else B
              where players = filter (/=B) (map has4 b ++ map has4 (transpose b) ++ map has4 (allDiagonals b))


-- Let's get artificially intelligent ---------------------------------------------------------------------------------
--
prep :: Board -> [(Player, [Board])]
prep b = [(whoWon b, [b])]

allMoves :: Board -> [Board]
allMoves b = [makeMove n (whoseGo b) b | n <- [0..cols-1]]

-- fellowAdj' [] _ _             = []                    -- a list of new Adjs based on the head of the Actor list of each one
-- fellowAdj' ((a, i):as) d done = let newFellows = [n | n <- allFellows (head a) d, not (elem n (done ++ a))]
--                                in [(new:a, i+1) | new <- newFellows] ++ fellowAdj' as d (newFellows ++ done)

allPoss2' :: [(Player, [Board])] -> [Board] -> [(Player, [Board])]
allPoss2' [] _          = []
allPoss2' ((p, bs):pbss) done = if p == B && (not . isFull . head) bs then let newList = [b | b <- allMoves (head bs), not (elem b (done ++ bs))]
                                                                           in [(whoWon b', b':bs) | b' <- newList] ++ allPoss2' pbss (newList ++ done)
                                                                      else allPoss2' pbss done

-- fellowAdj [] _ _    = []                            -- and reapplies fellowAdj' to that list, appending it to the first list
-- fellowAdj as d done = let newList = fellowAdj' as d done
--                       in newList ++ fellowAdj newList d (map (head . fst) newList ++ done)

allPoss2 :: [(Player, [Board])] -> [Board] -> [(Player, [Board])]
allPoss2 [] _      = []
allPoss2 pbss done = let newList = allPoss2' pbss done
                     in newList ++ allPoss2 newList (map (head . snd) newList ++ done)

--test :: [(Player, [Board])]
test n = let t = takeWhile ((<=n) . numPieces . head . snd) (allPoss2 (prep emptyBoard) [])
         in (length t) - ((length . map head . group . sort) t)

allPossible' :: [Board] -> [Board] -> [Board]
allPossible' [] _        = []
allPossible' (b:bs) done = let newList = [nb | nb <- allMoves b, not (elem nb done)]
                           in newList ++ allPossible' bs (done ++ newList)

allPossible :: [Board] -> [Board] -> [Board]
allPossible bs done = let newList = allPossible' bs done
                      in newList ++ allPossible newList (done ++ newList)

allPossLim :: [Board] -> [Board] -> Int -> [Board]
allPossLim bs done n = takeWhile ((<=n) . numPieces) (allPossible bs done)

genList b = allPoss2 (prep b) []

winMove :: Board -> (Player, [Board])
winMove b = (head . filter ((== whoseGo b) . fst)) (allPoss2 (prep b) [])

winLim :: Board -> [(Player, [Board])]
winLim b = takeWhile ((<=limit) . length . snd) (allPoss2 (prep b) [])

winLimFilt b = (filter ((== whoseGo b) . fst) . winLim) b


-- Actual full game stuff ---------------------------------------------------------------------------------------------

game' :: Board -> IO ()
game' b = do showBoard b
             let hasWon = whoWon b
             if hasWon /= B then putStrLn ("Congratulations " ++ show hasWon ++ ", you have won!")
                            else do let playerTurn = whoseGo b
                                    putStrLn ("Enter column, player " ++ show playerTurn ++ ":    ")
                                    x <- getLine
                                    game' (makeMove (toInt x) playerTurn b)

game :: IO ()
game = game' emptyBoard
