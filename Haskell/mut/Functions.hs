module Functions where

import Type, Data

numberOfEachTeam' :: Lineup -> [(Team, Int)]
numberOfEachTeam' =
  sortOn (Down . snd)
    . map (\arr -> (head arr, length arr))
    . group
    . sort
    . concatMap snd

rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

padRight :: Int -> Char -> String -> String
padRight l padding str = str ++ replicate (l - length str) padding

makeNumberHumanReadable :: Int -> String
makeNumberHumanReadable =
  reverse
    . intercalate ","
    . makeNumberHumanReadable'
    . reverse
    . show
  where
    makeNumberHumanReadable' [] = []
    makeNumberHumanReadable' xs =
      let (first, rest) = splitAt 3 xs
       in first : makeNumberHumanReadable' rest

avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 ns =
  (/ fromIntegral (length ns))
    . fromIntegral
    . sum
    . map (\n -> (\v -> min v (5 - v)) $ mod n 5)
    $ ns

bestCaptainOption :: Option -> Option
bestCaptainOption = head . bestCaptainOption'


bestCaptainOption' :: Option -> [Option]
bestCaptainOption' o =
  if captainTeam `elem` map fst o
    then
      let captainName = head . snd . head $ filter (\(t, _) -> t == captainTeam) o
          remaining = filter (\(t, _) -> t /= captainTeam) o
       in sortBy orderOptions . map (sortOn (Down . length . snd)) . testListFn (second (captainName :)) $ remaining
    else [o]

testListFn :: (Eq a) => (a -> a) -> [a] -> [[a]]
testListFn _ [] = []
testListFn f xs = testListFn' f xs xs
  where
    testListFn' _ [] cs = [cs]
    testListFn' _ _ [] = []
    testListFn' fn bs (c : cs) =
      let bs' = filter (/= c) bs
          newBs = fn c : bs'
       in (fn c : bs') : testListFn' fn bs cs

orderOptions :: Option -> Option -> Ordering
orderOptions tps1 tps2 =
  let lengths = map (length . snd) . take 3
   in orderOptions' (lengths tps1) (lengths tps2)

orderOptions' :: [Int] -> [Int] -> Ordering
orderOptions' xs ys =
  let sumComp = compare (sum xs) (sum ys)
   in if sumComp /= EQ
        then sumComp
        else compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)

reasonableModNumbers :: Int -> Int
reasonableModNumbers = (\x -> 10 ^ (x - 2)) . length . show

