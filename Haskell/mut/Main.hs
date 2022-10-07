import Type
import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord

squad :: Lineup
squad =
  concatMap
    (concatMap snd)
    [ offense,
      defense,
      specialTeams
    ]

numberOfEachTeam :: [(Team, Int)]
numberOfEachTeam = numberOfEachTeam' squad

numberOfEachTeam' :: Lineup -> [(Team, Int)]
numberOfEachTeam' =
  sortOn (Down . snd)
    . map (\arr -> (head arr, length arr))
    . group
    . sort
    . concatMap snd

{- HELPER FUNCTIONS -}

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

{- END HELPER FUNCTIONS -}

{- ACTUAL CALCULATIONS -}

numOptions :: Int
numOptions = product $ map (length . snd) team

team :: Lineup
team = map (DB.second sort) squad

allTeams :: [Team]
allTeams = (sort . rmDups . concatMap snd) team

allOptionsAlt :: Lineup -> [[(Player, Team)]]
allOptionsAlt = mapM (\(p, ts) -> [(p, t) | t <- ts])

longestTeamName :: Team
longestTeamName = maximumBy (comparing length) allTeams

longestTeamNameLength :: Int
longestTeamNameLength = length longestTeamName

allOptionsProcessed :: Lineup -> [Option]
allOptionsProcessed =
  map
    ( map (\(players, team) -> (head team, players))
        . sortOn (Down . length . snd)
        . map unzip
        . groupBy (\(_, t1) (_, t2) -> t1 == t2)
        . sortOn snd
    )
    . allOptionsAlt

allOptionsProcessedPrinting :: Lineup -> [IterationOrNumber]
allOptionsProcessedPrinting =
  allOptionsProcessedPrinting' (1, 1, [])
    . allOptionsProcessed

allOptionsProcessedPrinting' :: Iteration -> [Option] -> [IterationOrNumber]
allOptionsProcessedPrinting' it [] = [Iteration it]
allOptionsProcessedPrinting' it@(i, n, []) (a : as) =
  let bca = bestCaptainOption a
      bcaLength = sum . map (length . snd) . take 3 $ a
   in Iteration (i, bcaLength, bca) : allOptionsProcessedPrinting' (i + 1, bcaLength, bca) as
allOptionsProcessedPrinting' it@(i, n, o) (a : as) =
  let bca = bestCaptainOption a
      bcaLength = sum . map (length . snd) . take 3 $ bca
      next = i + 1
      notLarger = allOptionsProcessedPrinting' (next, n, o) as
   in if orderOptions o bca == GT
        then Iteration (i, bcaLength, bca) : allOptionsProcessedPrinting' (next, bcaLength, bca) as
        else
          if mod i (reasonableModNumbers numOptions) == 0
            then Number i : notLarger
            else notLarger

-- How far an array is from consisting of multiples of 5

ppIteration :: IterationOrNumber -> String
ppIteration (Iteration (iterationCount, maxValue, options)) =
  "Iteration "
    ++ makeNumberHumanReadable iterationCount
    ++ "\n"
    ++ ppOption options
ppIteration (Number n) =
  "Iteration"
    ++ " "
    ++ makeNumberHumanReadable n
    ++ " out of "
    ++ makeNumberHumanReadable numOptions
    ++ " ("
    ++ ( show
           . round
           . (100 *)
           . (\v -> fromIntegral n / v)
           . fromIntegral
       )
      numOptions
    ++ "%)"

ppOption :: Option -> String
ppOption =
  intercalate "\n"
    . map
      ( \(team, players) ->
          "    "
            ++ padRight longestTeamNameLength ' ' team
            ++ " | "
            ++ (show . length) players
            ++ " | "
            ++ intercalate ", " players
      )

ppOptions :: [Option] -> String
ppOptions = intercalate "\n\n" . map ppOption

foldOptionsFn :: Option -> Option -> Option
foldOptionsFn [] o = bestCaptainOption o
foldOptionsFn o1 o2 =
  let bco2 = bestCaptainOption o2
   in if orderOptions o1 bco2 == GT
      then o1
      else bco2

main :: IO ()
main =
  putStrLn
    . ppOption
    . foldl' foldOptionsFn []
    . allOptionsProcessed
    $ team
