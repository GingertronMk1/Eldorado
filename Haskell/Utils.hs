module Utils where
--
--
-- HASKELL UTILITIES:

import Data.List
import Data.Char
import Data.Maybe
import Data.Ord

flatten :: [[a]] -> [a]
flatten = concat

-- Starting with some Fibonacci stuff:

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsTo :: Int -> [Int]
fibsTo n = takeWhile (<n) fibs

nFibs :: Int -> [Int]
nFibs n = take n fibs

-- Random Listing Stuff

genList :: [Int]
genList = (concat . reverse . sequence . replicate 2) [15,14..0]

factorial :: Int -> Int
factorial n | n < 2 = 1
            | otherwise = n * factorial (n-1)

factorialAcca :: Int -> Int
factorialAcca n | n < 2 = 1
                | otherwise = factorialAcca' n 1
                where factorialAcca' 1 n = n
                      factorialAcca' x y = factorialAcca' (x-1) (x*y)

-- Factors

factorsOf :: Int -> [Int]
factorsOf n = [f | f <- [1..n], mod n f == 0]

isPrime :: Int -> Bool
isPrime n = factorsOf n == [1,n]

primesTo :: Int -> [Int]
primesTo n = [x | x <- [0..n], isPrime x]

-- Line by line: if `n` has no factors other than 1 and itself, it's prime and as such return a list of just it.
-- Otherwise, add to the list of factors (bar 1 and itself) the prime factors of n divided by each factor in turn

primeFactors :: Int -> [Int]
primeFactors n = case factors of [] -> [n]
                                 _  -> factors ++ primeFactors (div n (head factors))
                 where factors = take 1 [x | x <- factorsOf n, x /= 1 && x /= n]


intToList :: Int -> [Int]
intToList n = intToList' n []
              where intToList' 0 xs = xs
                    intToList' n xs = intToList' (div n 10) ((mod n 10):xs)

sumDigits :: Int -> Int
sumDigits n = sum (intToList n)

sumsTo :: Int -> [Int]
sumsTo n = [x | x <- [0..10^12], sumDigits x == n]

-- Quick 'n' Dirty function to get the closest int to a number's square root (rounding up at all times)

intSqrt :: Int -> Int
intSqrt n = intSqrt' n 0
            where intSqrt' n rt = if (rt*rt) < n then intSqrt' n (rt+1)
                                                 else rt

-- Sorting Algorithms
-- So many `Ord a` constraints

-- A utility function to determine whether or not a list is sorted

mySorted :: Ord a => [a] -> Bool
mySorted []       = True
mySorted [a]      = True
mySorted (a:b:as) = if a > b then False else mySorted (b:as)

-- Bubble-sort

bubble :: Ord a => [a] -> [a]
bubble ns = if mySorted lastRound then lastRound
                                  else bubble lastRound
            where lastRound = bubble' ns
                  bubble' x = case x of [] -> []
                                        (m:[]) -> [m]
                                        (m:n:ms) -> if m > n then n:(bubble' (m:ms))
                                                             else m:(bubble' (n:ms))


-- First, a test case
unsorted :: [Int]
unsorted = [1,10,2,9,3,8,4,7,5,6]

-- Insertion sort
insertion :: Ord a => [a] -> [a]
insertion []      = []
insertion [x]     = [x]
insertion (n:ns)  = myInsert n (insertion ns)

-- Selection sort
selection :: Ord a => [a] -> [a]
selection xs = selection' xs []
               where selection' [] bs = bs
                     selection' (c:cs) bs = selection' cs (myInsert c bs)

-- The actual insertion bit
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x < y     = x:y:ys
                  | otherwise = y:(myInsert x ys)


-- Speaking of whick...
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort f) (mergesort s)
               where f = take (div xsLength 2) xs
                     s = drop (div xsLength 2) xs
                     xsLength = length xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys))
                               else y:(merge (x:xs) ys)

-- Quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
                   where smallerSorted = quickSort (filter (<= x) xs)
                         biggerSorted  = quickSort (filter (> x) xs)

-- Image Compression Techniques

type Value = Int
type Length = Int

testRun :: [Value]
testRun = [1,1,2,2,2,3,3,4]


runlength :: [Value] -> [(Value, Length)]
runlength [] = []
runlength r@(x:xs) = runlength' r x 0
                     where runlength' [] v l = [(v,l)]
                           runlength' (a:as) v l = if a == v then runlength' as v (l+1)
                                                             else (v,l):(runlength' as a 1)

unrunlength :: [(Value, Length)] -> [Value]
unrunlength [] = []
unrunlength (t:ts) = (replicate l v) ++ (unrunlength ts)
                     where (v,l) = t

rltest :: [Int] -> [Int]
rltest = unrunlength . runlength

dpcm :: [Int] -> [Int]
dpcm ns = dpcm' ns 0
          where dpcm' [] _ = []
                dpcm' (n:ns) v = (n-v):(dpcm' ns n)

undpcm :: [Int] -> [Int]
undpcm ns = undpcm' ns 0
            where undpcm' [] _ = []
                  undpcm' (n:ns) v = let newN = n + v
                                      in newN:(undpcm' ns newN)

dpcmtest = undpcm . dpcm

-------------------------------

rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

sieve :: Int -> [Int]
sieve n = sieve' [1..n]
          where sieve' (x:xs) = if elem x xs then sieve' xs else xs


screenDims :: (Double, Double) -> Double -> (Double, Double)
screenDims (x, y) d = (a , a*r)
                      where r = x/y
                            a = sqrt (d*d / (1 + r*r))

--test :: [(String, Int)]
--test = [("Frank", 13),("Arthur", 14),("Frank", 3)]

times :: [(String, Int)] -> [(String, Int)]
times = reverse . sortBy (comparing snd) . props . accumulate . groupBy (\x y -> fst x == fst y) . sortBy (comparing fst)

accumulate :: [[(String, Int)]] -> [(String, Int)]
accumulate = map (\ss -> ((fst . head) ss, (sum . map snd) ss))

props :: [(String, Int)] -> [(String, Int)]
props ts = map (\(s, i) -> (s, quot (i*100) ((sum . map snd) ts))) ts

numOfEach n = let bigNums = [1..1000] in (putStr . flatten) [show (td2,td,fg) ++ "\n" | td2<-bigNums, td<-bigNums, fg<-bigNums, ((8*td2)+(7*td)+(3*fg))==n]

dartBoard :: [Int]
dartBoard = let oneTwenty = [0..20] in rmDups (oneTwenty ++ map (*2) oneTwenty ++ map (*3) oneTwenty ++ [25,50])



dartsScores' n = let sc = [(a,b,c) | a<-dartBoard, b<-dartBoard, c<-(50:[40,38..2]), a+b+c==n]
                     sn = show n
                 in if null sc then sn ++ "\n" ++ replicate (length sn) '-' ++ "\nFucked it"
                               else let (a,b,c) = head sc
                                    in sn ++ "\n" ++ replicate (length sn) '-' ++ "\n1:\t" ++ show a ++ "\n2:\t" ++ show b ++ "\n3:\t" ++ (if c == 50 then "Bull" else "double " ++ show (quot c 2))

dartsScores = (putStrLn . flatten . intersperse "\n\n" . map dartsScores') [0..180]

-- Added 2018-01-14 ------------------------------------------------------------

voucher :: Int
voucher = 600

cds = [("Robbie Williams - Greatest Hits",100),
       ("Lily Allen - It's Not Me It's You",100),
       ("Phil Collins - Face Value",300),
       ("Bryan Adams - The Best Of Me",300),
       ("Gnarls Barkley - St Elsewhere",20),
       ("Feeder - Echo Park",20),
       ("Linda Ronstadt - Greatest Hits",100)]

removeShorter' :: Eq b => (a -> b) -> [[a]] -> [[a]] -> [[a]]
removeShorter' f t []     = t
removeShorter' f t (a:as) = if (or . map (null . ((map f a)\\) . map f)) t
                            then removeShorter' f as t
                            else removeShorter' f as (a:t)

removeShorter :: Eq b => (a -> b) -> [[a]] -> [[a]]
removeShorter f as = removeShorter' f (removeShorter' f as []) []

cdO :: [[(String, Int)]]
cdO = (sortBy (comparing length) . removeShorter (fst) . rmDups . filter ((<=voucher) . sum . map snd)) [(sort . take n) l | l <- permutations cds, n <- [1..(length cds)]]

ppCDO = (putStrLn . flatten . intersperse "\n" . map (\ts -> ((show . sum . map snd) ts ++ "p:\t" ++ (flatten . intersperse ", " . map fst) ts))) cdO

-- Added 2018-01-24 ------------------------------------------------------------

elems :: Eq a => [a] -> [a] -> Bool
elems as = null . (as\\)

-- Added 2018-02-17, updated 2018-09-08 ----------------------------------------

seasonShows :: [(String, [Int])]
seasonShows = [
              ]

seasonGen :: [(String, [Int])] -> [[(String, Int)]]
seasonGen = filter (not . duplicates . map fst)
            . sequence
            . groupBy (\(_,as) (_,bs) -> as==bs)
            . sortBy (comparing snd)
            . flatten
            . map (\(a,bs) -> [(a,b) | b<-bs])

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (a:as) = if elem a as then True else duplicates as

seasons' :: [(String, [Int])] -> IO ()
seasons' [] = putStrLn "No shows provided"
seasons' a  = let longestName = ((+4) . length . last . sortBy (comparing length) . map fst) a
                  addPipe     = flatten . intersperse "| "
                  padding x   = replicate (longestName - (length x)) ' '
                  headerRow   = addPipe [let n' = "Slot " ++ show n in n' ++ padding n' | n<-(rmDups . flatten . map snd) a]
                  ppShows     = (flatten . intersperse "\n" . map (\ss -> addPipe [n ++ padding n | n<-(map fst) ss]) . seasonGen) a
               in putStrLn (headerRow ++ "\n" ++ replicate (length headerRow) '-' ++ "\n" ++ ppShows)

seasons :: IO ()
seasons = seasons' seasonShows


-- Added 2018-10-08

subarrays (n:[]) = [[n]]
subarrays (n:ns) = ((n:ns):subarrays ns)

-- Added 2018-11-05

fib' 0 = (0,0)
fib' 1 = (1,0)
fib' n = let (a,b) = fib' (n-1)
         in (a+b, a)


-- Added 2018--11-22

data Player = Player {x :: Int,
                      y :: Int,
                      vel :: (Int, Int)} deriving Eq

instance Show Player where
  show p = let px = (show . x) p
               py = (show . y) p
               vx = (show . fst . vel) p
               vy = (show . snd . vel) p
            in "x: " ++ px ++ ", y: " ++ py ++ ", vx: " ++ vx ++ ", vy: " ++ vy

testPlayer = Player 5 3 (-1, 2)
testPlayer2 = Player 5 3 (-1, 2)

playerUpdateX p = Player {x = x p + (fst . vel) p,
                          y = y p,
                          vel = vel p}

playerUpdateY p = Player {x = x p,
                          y = y p + (snd . vel) p,
                          vel = vel p}

playerUpdate = playerUpdateX . playerUpdateY

-- Added 2019-01-15

data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

ppTree :: (Show a) => Tree a -> IO()
ppTree = putStr . showTree

showTree :: (Show a) => Tree a -> String
showTree = showTree' 0
           where showTree' indent Leaf = ""
                 showTree' indent (Node x l r) = replicate indent ' '
                                                 ++ show x ++ "\n"
                                                 ++ showTree' (indent+1) l
                                                 ++ showTree' (indent+1) r

limitTree :: Int -> Tree a -> Tree a
limitTree 0 (Node x _ _) = Node x Leaf Leaf
limitTree n (Node x l r) = Node x (limitTree (n-1) l) (limitTree (n-1) r)

treeHeight :: Tree a -> Int
treeHeight = treeHeight' 0
             where treeHeight' n Leaf = (n-1)
                   treeHeight' n (Node _ l r) = larger (treeHeight' (n+1) l) (treeHeight' (n+1) r)
                   larger x y = if x >= y then x else y

bst :: Tree Int
bst = Node 8
        (Node 4
           (Node 2
              (Node 1 Leaf Leaf)
              (Node 3 Leaf Leaf))
           (Node 6
              (Node 5 Leaf Leaf)
              (Node 7 Leaf Leaf)))
        (Node 12
           (Node 10
              (Node 9 Leaf Leaf)
              (Node 11 Leaf Leaf))
           (Node 14
              (Node 13 Leaf Leaf)
              (Node 15 Leaf Leaf)))

testTree :: Tree Int
testTree = Node 1
             (Node 2
                (Node 4 Leaf Leaf)
                (Node 5 Leaf Leaf))
             (Node 3 Leaf Leaf)

treeGen :: Int -> Tree Int
treeGen l = let n = 1
             in limitTree l $ treeGen' n
            where treeGen' x = Node x (treeGen' (x+1)) (treeGen' (x+1))

treeBFS :: Tree a -> [a]
treeBFS = reverse . treeBFS' [] . (:[])
          where treeBFS' ns [] = ns
                treeBFS' ns (Leaf : q) = treeBFS' ns q
                treeBFS' ns (Node x l r : q) = treeBFS' (x:ns) (q ++ [l] ++ [r])

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r


postOrder :: Tree a -> [a]
postOrder Leaf = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]

preOrder :: Tree a -> [a]
preOrder Leaf = []
preOrder (Node x l r) = [x] ++ preOrder l ++ preOrder r

-- Added 2019-01-18

type FIFO a = ([a],[a])

emptyFIFO :: FIFO a
emptyFIFO = ([],[])

toFIFO :: [a] -> FIFO a
toFIFO xs = let (f', r') = splitAt (div (length xs) 2) xs
             in (f', reverse r')

fromFIFO :: FIFO a -> [a]
fromFIFO (fs, rs) = fs ++ reverse rs

fFix :: FIFO a -> FIFO a
fFix ([], rs) = let (r', f') = splitAt (div (length rs) 2) rs
                 in (reverse f', r')
fFix f@(_,_) = f


fAdd :: a -> FIFO a -> FIFO a
fAdd x (fs, rs) = (fs, x:rs)

fDrop :: FIFO a -> FIFO a
fDrop (f:[], rs) = fFix ([], rs)
fDrop (f:fs, rs) = (fs, rs)

fExtract :: FIFO a -> (a, FIFO a)
fExtract f@([], rs) = fExtract $ fFix f
fExtract (f:fs, rs) = (f, (fs, rs))

fHead (f:[], rs) = fFix ([], rs)
fHead (f:fs, rs) = (fs, rs)

treeBFSFIFO :: Tree a -> [a]
treeBFSFIFO = (reverse . treeBFSFIFO' [] . (\t -> fAdd t ([],[])))
              where treeBFSFIFO' ns ([],[]) = ns
                    treeBFSFIFO' ns q = case (fExtract q) of (Leaf, q')       -> treeBFSFIFO' ns q'
                                                             (Node x l r, q') -> treeBFSFIFO' (x:ns) ((fAdd r . fAdd l) q')
