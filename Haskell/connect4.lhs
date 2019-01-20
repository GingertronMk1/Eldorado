-- THE GAME CONNECT-4
-- BUT, LIKE, HASKELLY
-- TODO: FUCKING ORDER AND TIDY THIS

import Data.List (group, transpose)
import System.Random (randomRIO)

rows :: Int
rows = 6    -- Standard: 6

cols :: Int
cols = 7    -- Standard: 7

win :: Int
win = 4     -- Standard: 4

depth :: Int
depth = 5   -- Standard: 6

type Board = [Row]

type Row = [Player]

data Player = O | B | X
              deriving (Eq, Ord, Show)

showBoard :: Board -> IO ()
showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
              where width   = boardWidth b
                    height  = boardHeight b
                    showRow = map showPlayer
                    line    = replicate width '-'
                    nums    = take width ['1'..]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'

boardGen :: Int -> Int -> Board
boardGen w h = replicate h (replicate w B)

initBoard :: Board
initBoard = boardGen cols rows

-- WHO HAS WON

boardHeight = length
boardWidth = length . head

rowsToCols :: Board -> Board
rowsToCols = transpose

colsToRows :: Board -> Board
colsToRows = transpose

toDiags :: Board -> Board
toDiags b = toDiags' b ++ toDiags' (map reverse b)
            where toDiags' b = let noDiags = (length b) + (length (head b)) - 1
                                in [d | d <- map (getDiags b) [0..noDiags], length d >= win]
                  getDiags b = map (\(y,x) -> (b !! y) !! x) . diags b

diags :: Board -> Int -> [(Int, Int)]
diags b n = (filter (\(y,x) -> y < boardHeight b && x < boardWidth b) . take n . diags') (0,n-1)
            where diags' (y,x) = (y,x) : diags' (y+1, x-1)

-- MAKING MOVES

makeMove :: Player -> Int -> Board -> Board
makeMove p n b = let columns = rowsToCols b
                     colsBefore = take n columns
                     colsAfter = drop (n+1) columns
                     newCol = (fillCol b . (p:) . dropWhile (==B) . (!!n)) columns
                  in colsToRows $ colsBefore ++ [newCol] ++ colsAfter
                 where fillCol b c = if length c < boardHeight b then fillCol b (B:c) else c

numPieces :: Board -> Int
numPieces = length . filter (/=B) . concat

isFull :: Board -> Bool
isFull = not . elem B . concat

isColFull :: Row -> Bool
isColFull = not . elem B

isValid :: Int -> Board -> Bool
isValid n b = elem n [0..length (head b) - 1] && (elem B . (!!n) . rowsToCols) b

-- WINNING

hasXWon :: Row -> Bool
hasXWon = elem (replicate win X) . group

hasOWon :: Row -> Bool
hasOWon = elem (replicate win O) . group

hasWonRow :: Row -> Player
hasWonRow r | hasXWon r = X
            | hasOWon r = O
            | otherwise = B

whoWon :: Board -> Player
whoWon b | elem X b' = X
         | elem O b' = O
         | otherwise = B
         where b' = let cs = rowsToCols b
                        ds = toDiags b
                     in map hasWonRow (b ++ cs ++ ds)

-- HELPERS

noPieces :: Player -> Board -> Int
noPieces p = length . filter (==p) . concat

whoseGo :: Board -> Player
whoseGo b = if noPieces X b > noPieces O b then O else X

next :: Player -> Player
next X = O
next O = X
next B = B

-- TREE THINGS

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

gameTree :: Board -> Player -> Tree Board
gameTree b p = Node b [gameTree b' (next p) | b' <- moves b p]

moves :: Board -> Player -> [Board]
moves b p | whoWon b /= B = []
          | isFull b      = []
          | otherwise     = [makeMove p n b | n <- [0..length (head b) - 1], isValid n b]

limitTree :: Int -> Tree a -> Tree a
limitTree 0 (Node x _)  = Node x []
limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

limitedTree :: Board -> Tree Board
limitedTree = limitTree depth . (\b -> gameTree b (whoseGo b))

extendTree :: Tree Board -> Tree Board
extendTree (Node b []) = Node b [Node b' [] | b' <- (moves b (whoseGo b))]
extendTree (Node b ts) = Node b (map extendTree ts)

initTree :: Tree Board
initTree = limitedTree initBoard

-- MINMAX ALGORITHM

minMax :: Tree Board -> Tree (Board, Player)
minMax (Node b []) = Node (b, whoWon b) []
minMax (Node b ts) = case (whoseGo b) of O -> Node (b, minimum ps) ts'
                                         X -> Node (b, maximum ps) ts'
                     where ts' = map minMax ts
                           ps = [p | Node (_,p) _ <- ts']

bestMove :: Tree Board -> Tree (Board, Player)
bestMove t = head [Node (b',p') ts' | Node (b', p') ts' <- ts, p' == best]
             where Node (_, best) ts = minMax t

-- IO THINGS LEADING UP TO PLAYING THE GAME

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then return $ read xs
                   else do putStrLn "ERROR: Invalid number"
                           getNat prompt
                where isDigit c = elem (read [c] :: Int) [0..9]

pve :: Player -> Tree Board -> IO()
pve p (Node b ts) = case p of O -> do showBoard b
                                      putStrLn "Thinking...\n"
                                      r <- randomRIO (0, length ts - 1)
                                      let Node (b', p') ts' = bestMove (Node b ts)
                                          t' = if p' /= B then head $ filter (\(Node x xs) -> x == b') ts
                                                          else ts!!r
                                      if whoWon b' == p then do putStrLn "The computer wins"
                                                                showBoard b'
                                                        else do putStrLn $ show p'
                                                                pve X (extendTree t')
                              X -> do showBoard b
                                      n <- getNat "Please enter your column number: "
                                      putStrLn ""
                                      let b' = makeMove X (n-1) b
                                          t' = head $ filter (\(Node x xs) -> x == b') ts
                                      if whoWon b' == X then do putStrLn "You win!"
                                                                showBoard b'
                                                        else pve O (extendTree t')
                              otherwise -> putStrLn "HOW?"

pvp :: Player -> Board -> IO()
pvp p b = do showBoard b
             n <- getNat $ show p ++ "'s turn. Please enter your column number: "
             let b' = makeMove p (n-1) b
             if (whoWon b') == p then do putStrLn $ show p ++ " wins!"
                                         showBoard b'
                                 else pvp (next p) b'

eve :: Player -> Tree Board -> IO()
eve p (Node b ts) = do showBoard b
                       putStrLn "Thinking...\n"
                       r <- randomRIO (0, length ts - 1)
                       let Node (b', p') ts' = bestMove (Node b ts)
                           t' = if p' /= B then head $ filter (\(Node x xs) -> x == b') ts
                                           else ts!!r
                       if whoWon b' == p then do putStrLn $ "The computer playing " ++ show p ++ " wins"
                                                 showBoard b'
                                         else do putStrLn $ show p'
                                                 eve (next p) (extendTree t')

main :: IO()
main = do n <- getNat "Press 1 to play against a human\nPress 2 to play against the computer\nPress 3 to watch the computer play itself\n"
          let firstGo = whoseGo initBoard
          case n of 1 -> do putStrLn "You have selected PvP"
                            pvp firstGo initBoard
                    2 -> do putStrLn "You have selected PvE"
                            pve firstGo initTree
                    3 -> do putStrLn "You have selected EvE"
                            eve firstGo initTree
