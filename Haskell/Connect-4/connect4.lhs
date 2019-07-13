THE GAME CONNECT-4
BUT, LIKE, HASKELLY
TODO: FUCKING ORDER AND TIDY THIS

A couple of useful imports:
- group is used for win functions
- transpose is used to get the columns out of the board (and put them back)
- randomRIO is used when the minMax can't find a best course of action, so it makes a random move

> import Data.List (group, transpose)
> import System.Random (randomRIO)

The number of rows the standard board should have
By default it should be 6

> rows :: Int
> rows = 6

The number of columns the standard board should have
By default it should be 7

> cols :: Int
> cols = 7

How many tokens in a row is a win
By default it should be 4 (clue's in the name really)

> win :: Int
> win = 4

How many moves ahead the game tree should be generated
By default it should be 6

> depth :: Int
> depth = 6

Players are denoted by their token type, Xs and Os

> data Player = O
>             | B
>             | X
>               deriving (Eq, Ord, Show)

A row is a list of players, and a board is a list of rows

> type Row = [Player]
> type Board = [Row]

Nicely printing the board now

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where width   = boardWidth b
>                     height  = boardHeight b
>                     showRow = map showPlayer
>                     line    = replicate width '-'
>                     nums    = take width ['1'..]
>                     showPlayer O = 'O'
>                     showPlayer B = '.'
>                     showPlayer X = 'X'

Generating a board of a certain size

> boardGen :: Int -> Int -> Board
> boardGen w h = replicate h (replicate w B)

A board of the size we want

> initBoard :: Board
> initBoard = boardGen cols rows

--------------------------------------------------------------------------------
WHO HAS WON---------------------------------------------------------------------
--------------------------------------------------------------------------------

Finding the height and width of a board

> boardHeight :: Board -> Int
> boardHeight = length

> boardWidth :: Board -> Int
> boardWidth = length . head

Turning rows into columns

> rowsToCols :: Board -> Board
> rowsToCols = transpose

Turning them back (mostly for clarity's sake this one)

> colsToRows :: Board -> Board
> colsToRows = transpose

Now we gotta find the diagonals
This is a bit tricky tbh

> rowsToDiags :: Board -> Board
> rowsToDiags b = rowsToDiags' b ++ rowsToDiags' (map reverse b)
>                 where rowsToDiags' b = let noDiags = (length b) + (length (head b)) - 1
>                                         in [d | d <- map (getDiags b) [0..noDiags], length d >= win]
>                       getDiags b = map (\(y,x) -> (b !! y) !! x) . diags b

> diags :: Board -> Int -> [(Int, Int)]
> diags b n = (filter (\(y,x) -> y < boardHeight b && x < boardWidth b) . diags') (0,n-1)
>             where diags' (y,0) = []
>                   diags' (y,x) = (y,x) : diags' (y+1, x-1)

--------------------------------------------------------------------------------
WINNING-------------------------------------------------------------------------
--------------------------------------------------------------------------------

Has X won the row?

> hasXWon :: Row -> Bool
> hasXWon = elem (replicate win X) . group

Has Y won the row?

> hasOWon :: Row -> Bool
> hasOWon = elem (replicate win O) . group

Who has won the row?

> hasWonRow :: Row -> Player
> hasWonRow r | hasXWon r = X
>             | hasOWon r = O
>             | otherwise = B

Who has won the entire board?

> whoWon :: Board -> Player
> whoWon b | elem X b' = X
>          | elem O b' = O
>          | otherwise = B
>          where b' = map hasWonRow (b ++ rowsToCols b ++ rowsToDiags b)

--------------------------------------------------------------------------------
MAKING MOVES--------------------------------------------------------------------
--------------------------------------------------------------------------------

Making a move given a player, a column number, and the board

> makeMove :: Player -> Int -> Board -> Board
> makeMove p n b = let columns = rowsToCols b
>                      colsBefore = take n columns
>                      colsAfter = drop (n+1) columns
>                      newCol = (fillCol b . (p:) . dropWhile (==B) . (!!n)) columns
>                   in colsToRows $ colsBefore ++ [newCol] ++ colsAfter
>                  where fillCol b c = if length c < boardHeight b then fillCol b (B:c) else c

Is this column full?

> isColFull :: Row -> Bool
> isColFull = not . elem B

Is the board full?

> isFull :: Board -> Bool
> isFull = isColFull . concat

Is a given move attempt valid?

> isValid :: Int -> Board -> Bool
> isValid n b = elem n [0..length (head b) - 1] && (elem B . (!!n) . rowsToCols) b

--------------------------------------------------------------------------------
HELPERS-------------------------------------------------------------------------
--------------------------------------------------------------------------------

The number of pieces of a certain player on the board

> noPieces :: Player -> Board -> Int
> noPieces p = length . filter (==p) . concat

Whose go is it?

> whoseGo :: Board -> Player
> whoseGo b = if noPieces X b > noPieces O b then O else X

"Incrementing" the player

> next :: Player -> Player
> next O = X
> next B = B
> next X = O

--------------------------------------------------------------------------------
TREE THINGS---------------------------------------------------------------------
--------------------------------------------------------------------------------

> data Tree a = Node a [Tree a]
>               deriving (Eq, Show)

> gameTree :: Board -> Player -> Tree Board
> gameTree b p = Node b [gameTree b' (next p) | b' <- moves b p]

> moves :: Board -> Player -> [Board]
> moves b p | whoWon b /= B = []
>           | isFull b      = []
>           | otherwise     = [makeMove p n b | n <- [0..length (head b) - 1], isValid n b]

> limitTree :: Int -> Tree a -> Tree a
> limitTree 0 (Node x _)  = Node x []
> limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

> limitedTree :: Board -> Tree Board
> limitedTree = limitTree depth . (\b -> gameTree b (whoseGo b))

> extendTree :: Tree Board -> Tree Board
> extendTree (Node b []) = Node b [Node b' [] | b' <- (moves b (whoseGo b))]
> extendTree (Node b ts) = Node b (map extendTree ts)

> initTree :: Tree Board
> initTree = limitedTree initBoard

--------------------------------------------------------------------------------
MINMAX ALGORITHM----------------------------------------------------------------
--------------------------------------------------------------------------------

> minMax :: Tree Board -> Tree (Board, Player)
> minMax (Node b []) = Node (b, whoWon b) []
> minMax (Node b ts) = case (whoseGo b) of O -> Node (b, minimum ps) ts'
>                                          X -> Node (b, maximum ps) ts'
>                      where ts' = map minMax ts
>                            ps = [p | Node (_,p) _ <- ts']

> bestMove :: Tree Board -> Tree (Board, Player)
> bestMove t = head [Node (b',p') ts' | Node (b', p') ts' <- ts, p' == best]
>              where Node (_, best) ts = minMax t

--------------------------------------------------------------------------------
IO THINGS LEADING UP TO PLAYING THE GAME----------------------------------------
--------------------------------------------------------------------------------

> getNat :: String -> IO Int
> getNat prompt = do putStr prompt
>                    xs <- getLine
>                    if xs /= [] && all isDigit xs then return $ read xs
>                    else do putStrLn "ERROR: Invalid number"
>                            getNat prompt
>                 where isDigit c = elem (read [c] :: Int) [0..9]

> pve :: Player -> Tree Board -> IO()
> pve p (Node b ts) = case p of O -> do showBoard b
>                                       putStrLn "Thinking...\n"
>                                       r <- randomRIO (0, length ts - 1)
>                                       let Node (b', p') ts' = bestMove (Node b ts)
>                                           t' = if p' /= B then head $ filter (\(Node x xs) -> x == b') ts
>                                                           else ts!!r
>                                       if whoWon b' == p then do putStrLn "The computer wins"
>                                                                 showBoard b'
>                                                         else pve X (extendTree t')
>                               X -> do showBoard b
>                                       n <- getNat "Please enter your column number: "
>                                       putStrLn ""
>                                       let b' = makeMove X (n-1) b
>                                           t' = head $ filter (\(Node x xs) -> x == b') ts
>                                       if whoWon b' == X then do putStrLn "You win!"
>                                                                 showBoard b'
>                                                         else pve O (extendTree t')
>                               otherwise -> putStrLn "HOW?"

> pvp :: Player -> Board -> IO()
> pvp p b = do showBoard b
>              n <- getNat $ show p ++ "'s turn. Please enter your column number: "
>              let b' = makeMove p (n-1) b
>              if (whoWon b') == p then do putStrLn $ show p ++ " wins!"
>                                          showBoard b'
>                                  else pvp (next p) b'

> eve :: Player -> Tree Board -> IO()
> eve p (Node b ts) = do showBoard b
>                        putStrLn "Thinking...\n"
>                        r <- randomRIO (0, length ts - 1)
>                        let Node (b', p') ts' = bestMove (Node b ts)
>                            t' = if p' /= B then head $ filter (\(Node x xs) -> x == b') ts
>                                            else ts!!r
>                            t'' = extendTree t'
>                        if whoWon b' == p then do putStrLn $ "The computer playing " ++ show p ++ " wins"
>                                                  showBoard b'
>                                          else eve (next p) t''

--------------------------------------------------------------------------------
FINALLY, MAIN-------------------------------------------------------------------
--------------------------------------------------------------------------------

> main :: IO()
> main = do n <- getNat "Press 1 to play against a human\nPress 2 to play against the computer\nPress 3 to watch the computer play itself\n"
>           let firstGo = whoseGo initBoard
>           case n of 1 -> do putStrLn "You have selected PvP"
>                             pvp firstGo initBoard
>                     2 -> do putStrLn "You have selected PvE"
>                             pve firstGo initTree
>                     3 -> do putStrLn "You have selected EvE"
>                             eve firstGo initTree
