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
avgDistanceFromMultiplesOf5 = mean . map distanceFrom5

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
       in sortBy orderOptions . map (sortOn (Down . length . snd)) . iterativeApplicationFn (DB.second (captainName :)) $ remaining
    else [o]

-- Apply a function to each item in a list, returning a list of lists containing
-- the result of each application
iterativeApplicationFn :: (Eq a) => (a -> a) -> [a] -> [[a]]
iterativeApplicationFn _ [] = []
iterativeApplicationFn f xs = iterativeApplicationFn' f xs xs

iterativeApplicationFn' :: Eq a => (a -> a) -> [a] -> [a] -> [[a]]
iterativeApplicationFn' _ [] cs = [cs]
iterativeApplicationFn' _ _ [] = []
iterativeApplicationFn' fn bs (c : cs) =
  let bs' = filter (/= c) bs
   in (fn c : bs') : iterativeApplicationFn' fn bs' cs

-- Returns whether tps2 should be higher up the list than tps1
orderOptions :: Option -> Option -> Ordering
orderOptions o1 o2 =
  let lengths = map (length . snd) . take 3
   in fst $ orderOptions' (lengths o1) (lengths o2)

orderOptions' :: [Int] -> [Int] -> (Ordering, String)
orderOptions' xs ys
  | sumComp /= EQ = (sumComp, "Sum")
  | numComp /= EQ = (numComp, "5s")
  | otherwise = (distComp, "Dist")
  where
    sumComp = compare (sum xs) (sum ys)
    num5s = length . filter (== 0) . map (`mod` 5)
    numComp = compare (num5s xs) (num5s ys)
    distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)

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
      largestNumber = length . show . maximum . map (length . snd) $ o
      indent = replicate 2 ' '
   in intercalate "\n"
        . map
          ( \(team, p@(player : players)) ->
              indent
                ++ padRight longestTeamNameLength ' ' team
                ++ " | "
                ++ padRight largestNumber ' ' ((show . length) p)
                ++ " | "
                ++ player
                ++ concatMap
                  ( \player' ->
                      "\n"
                        ++ indent
                        ++ replicate longestTeamNameLength ' '
                        ++ " | "
                        ++ replicate largestNumber ' '
                        ++ " | "
                        ++ player'
                  )
                  players
          )
        $ o

ppOptions :: [Option] -> String
ppOptions = intercalate "\n\n" . map ppOption

putPPOptions :: [Option] -> IO ()
putPPOptions = putStrLn . ppOptions

popularitySort :: Lineup -> Lineup
popularitySort l = map (DB.second $ sortBy $ popSort' l) l

popSort' :: Lineup -> Team -> Team -> Ordering
popSort' l t1 t2 = compare (popSort'' t2 l) (popSort'' t1 l)

popSort'' :: Team -> Lineup -> Int
popSort'' t = length . filter (== t) . concatMap snd

numOptionsFn :: Lineup -> Int
numOptionsFn = product . map (length . snd)

allTeamsFn :: Lineup -> [Team]
allTeamsFn = rmDups . concatMap snd

popFilter :: Lineup -> Lineup
popFilter l =
  let allTeams = concatMap snd l
      numOfOneTeam t = length . filter (t==) $ allTeams
      filterTeamList = filter (\t -> numOfOneTeam t > 4 || t == captainTeam)
   in filter (not . null . snd) . map (DB.second filterTeamList) $ l

lineupToPlayerTeams :: Lineup -> [[(Player, Team)]]
lineupToPlayerTeams = mapM (\(p, ts) -> [(p, t) | t <- ts])

foldFunction :: [Option] -> [Option]
foldFunction [] = []
foldFunction os = foldFunction' os []

foldFunction' :: [Option] -> Option -> [Option]
foldFunction' [] _ = []
foldFunction' (o : os) biggestO =
  if orderOptions o biggestO == GT
    then o : foldFunction' os o
    else foldFunction' os biggestO

mean :: (Integral a) => [a] -> Float
mean ls = fromIntegral (length ls) / fromIntegral (sum ls)

{- Useful for debugging/testing -}

ppSquad :: Lineup -> IO()
ppSquad = 
  putStrLn
  . intercalate "\n"
  . map (\(p, ts) -> p ++ ": " ++ intercalate ", " ts)