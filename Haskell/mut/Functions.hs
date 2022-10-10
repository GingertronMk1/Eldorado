module Functions where

import Control.Arrow
import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Type

numberOfEachTeam' :: Lineup -> [(Team, Int)]
numberOfEachTeam' =
  sortOn (Down . snd)
    . map (head Control.Arrow.&&& length)
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
      let (firstNum, restNums) = splitAt 3 xs
       in firstNum : makeNumberHumanReadable' restNums

avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 ns =
  (/ fromIntegral (length ns))
    . fromIntegral
    . sum
    . map distanceFrom5
    $ ns

distanceFrom5 :: Int -> Int
distanceFrom5 n = (\v -> min v (5 - v)) $ mod n 5

bestCaptainOption :: Option -> Option
bestCaptainOption = last . bestCaptainOption'

bestCaptainOption' :: Option -> [Option]
bestCaptainOption' o =
  if captainTeam `elem` map fst o
    then
      let captainName = head . snd . head $ filter (\(t, _) -> t == captainTeam) o
          remaining = filter (\(t, _) -> t /= captainTeam) o
       in sortBy orderOptions . map (sortOn (Down . length . snd)) . testListFn (DB.second (captainName :)) $ remaining
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

-- Returns whether tps2 should be higher up the list than tps1
orderOptions :: Option -> Option -> Ordering
orderOptions o1 o2 =
  let lengths = map (length . snd) . take 3
   in fst $ orderOptions' (lengths o1) (lengths o2)

orderOptions' :: [Int] -> [Int] -> (Ordering, String)
orderOptions' xs ys =
  let sumComp = compare (sum xs) (sum ys)
      distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)
      num5s = length . filter (== 0) . map (`mod` 5)
      numComp = compare (num5s xs) (num5s ys)
   in if sumComp /= EQ
        then (sumComp, "Sum")
        else
          if numComp /= EQ
            then (numComp, "5s")
            else (distComp, "Dist")

reasonableModNumbers :: Int -> Int
reasonableModNumbers = (\x -> 10 ^ (x - 2)) . length . show

playerTeamToOption :: [(Player, Team)] -> Option
playerTeamToOption =
  bestCaptainOption
    . sortOn (Down . length . snd)
    . map
      ( (\(ps, t : _) -> (t, ps))
          . unzip
      )
    . groupBy (\(_, t1) (_, t2) -> t1 == t2)
    . sortOn snd

ppOption :: Option -> String
ppOption o =
  let longestTeamNameLength = length . maximumBy (comparing length) . map fst $ o
   in intercalate "\n"
        . map
          ( \(team, players) ->
              "    "
                ++ padRight longestTeamNameLength ' ' team
                ++ " | "
                ++ (show . length) players
                ++ " | "
                ++ intercalate ", " players
          )
        $ o

ppOptions :: [Option] -> String
ppOptions = intercalate "\n\n" . map ppOption

putPPOptions :: [Option] -> IO ()
putPPOptions = putStrLn . ppOptions