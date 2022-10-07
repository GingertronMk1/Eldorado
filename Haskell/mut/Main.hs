import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Functions
import Type

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
