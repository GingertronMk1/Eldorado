import Data.Bifunctor as DB
import Data.List
import Data.Ord

type Team = String

type Position = String

type Player = String

type PlayerTeams = (Player, [Team])

type Lineup = [PlayerTeams]

type LiterateLineup = [(Position, Lineup)]

type TeamPlayer = (Team, [Player])

type Option = [TeamPlayer]

type IterationCount = Int

type NumInTeam = Int

type Iteration = (IterationCount, NumInTeam, Option)

data IterationOrNumber
  = Iteration Iteration
  | Number IterationCount

{- THE TEAM DATA -}

offense :: LiterateLineup
offense =
  [ ( "qb",
      [ ("Justin Fields", ["Bears"]),
        ("Tim Tebow", ["Broncos", "Jets", "Legends"])
      ]
    ),
    ( "hb",
      [ ("Cordarrelle Patterson", ["Bears", "Falcons", "Raiders", "Patriots", "Vikings"]),
        ("Demarco Murray", ["Cowboys", "Eagles", "Titans", "Legends"]),
        ("Mark Ingram II", ["Saints", "Ravens", "Texans"])
      ]
    ),
    ( "fb",
      [ ("Jim Taylor", ["Packers", "Saints", "Legends"]),
        ("Reggie Gilliam", ["Bills"])
      ]
    ),
    ( "te",
      [ ("Dave Casper", ["Raiders", "Titans", "Vikings", "Legends"]),
        ("Evan Engram", ["Giants", "Jaguars"]),
        ("Dawson Knox", ["Bills"])
      ]
    ),
    ( "wr",
      [ ("DJ Moore", ["Panthers"]),
        ("Brandin Cooks", ["Patriots", "Rams", "Saints", "Texans"]),
        ("Devante Parker", ["Patriots", "Dolphins"]),
        ("Ceedee Lamb", ["Cowboys"]),
        ("Gabe Davis", ["Bills"])
      ]
    ),
    ( "lt",
      [ ("Orlando Brown", ["Chiefs", "Ravens"]),
        ("Terron Armstead", ["Dolphins", "Saints"])
      ]
    ),
    ( "lg",
      [ ("Isaac Seumalo", ["Eagles"]),
        ("Frank Ragnow", ["Lions"])
      ]
    ),
    ( "c",
      [ ("Tyler Shatley", ["Jaguars"])
      ]
    ),
    ( "rg",
      [ ("Nate Davis", ["Titans"])
      ]
    ),
    ( "rt",
      [ ("Lane Johnson", ["Eagles"])
      ]
    )
  ]

defense :: LiterateLineup
defense =
  [ ( "mlb",
      [ ("Isaiah Simmons", ["Cardinals"]),
        ("Tremaine Edmunds", ["Bills"]),
        ("Zaven Collins", ["Cardinals"])
      ]
    ),
    ( "rolb",
      [ ("Divine Deablo", ["Raiders"])
      ]
    ),
    ( "lolb",
      [ ("Jalen Reeves-Maybin", ["Lions", "Texans"])
      ]
    ),
    ( "ss",
      [ ("Harrison Smith", ["Vikings"]),
        ("Grant Delpit", ["Browns"])
      ]
    ),
    ( "fs",
      [ ("Trevon Moehrig", ["Raiders"]),
        ("Budda Baker", ["Cardinals"])
      ]
    ),
    ( "cb",
      [ ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
        ("Sauce Gardner", ["Jets"]),
        ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
        ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"]),
        ("Randy Moss", ["Raiders", "Patriots", "Titans", "Legends", "Vikings", "49ers"])
      ]
    ),
    ( "dt",
      [ ("Sam Adams", ["CAPTAIN"]),
        ("Deforest Buckner", ["49ers", "Colts"]),
        ("Derrick Brown", ["Panthers"])
      ]
    )
  ]

specialTeams :: LiterateLineup
specialTeams =
  [ ( "k",
      [ ("Justin Reid", ["Texans", "Chiefs"])
      ]
    ),
    ( "p",
      [ ("Sterling Hofrichter", ["Dolphins"])
      ]
    )
  ]

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

allOptions :: Lineup -> [[(Player, Team)]]
allOptions t = map (zip (map fst t)) . mapM snd $ t

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
allOptionsProcessedPrinting' it@(i, n, o) (a : as) =
  let aLengthVals = map (length . snd) . take 3 $ a
      aLength = sum aLengthVals
      next = i + 1
      notLarger = allOptionsProcessedPrinting' (next, n, o) as
   in if aLength > n || (aLength == n && allOptionsProcessedPrinting'' a o)
        then Iteration (i, aLength, a) : allOptionsProcessedPrinting' (next, aLength, a) as
        else
          if mod i (div numOptions 100) == 0
            then Number i : notLarger
            else notLarger

allOptionsProcessedPrinting'' :: Option -> Option -> Bool
allOptionsProcessedPrinting'' tps1 tps2 =
  stDevOfOption tps1 < stDevOfOption tps2
  where
    stDevOfOption =
      avgDistanceFromMultiplesOf5 . map (length . snd) . take 3

-- How far an array is from consisting of multiples of 5
avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 ns =
  (/ fromIntegral (length ns))
    . fromIntegral
    . sum
    . map (\n -> (\v -> min v (5 - v)) $ mod n 5)
    $ ns

ppIteration :: IterationOrNumber -> String
ppIteration (Iteration (iterationCount, maxValue, options)) =
  "Iteration "
    ++ makeNumberHumanReadable iterationCount
    ++ "\n"
    ++ ( intercalate "\n"
           . map
             ( \(team, players) ->
                 "    "
                   ++ padRight longestTeamNameLength ' ' team
                   ++ " | "
                   ++ (show . length) players
                   ++ " | "
                   ++ intercalate ", " players
             )
       )
      options
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

main :: IO ()
main =
  putStrLn
    . intercalate "\n\n"
    . map ppIteration
    . allOptionsProcessedPrinting
    $ team
