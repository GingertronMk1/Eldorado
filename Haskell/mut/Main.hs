import Data.Bifunctor as DB
import Data.List
import Data.Ord

type Team = String

type Position = String

type Player = String

type PlayerTeam = (Player, [Team])

type Lineup = [PlayerTeam]

type LiterateLineup = [(Position, Lineup)]

type TeamPlayer = (Team, [Player])

type Option = [TeamPlayer]

type IterationCount = Int

type Iteration = (IterationCount, Int, Option)

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
      [ ("Jim Taylor", ["Packers", "Saints", "Legends"])
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
        ("Zaven Collins", ["Cardinals"]),
        ("Jordyn Brooks", ["Seahawks"])
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
        ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
        ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"]),
        ("Mike Hilton", ["Bengals", "Steelers"]),
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

rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

oneList :: [a] -> [b] -> [(a, b)]
oneList _ [] = []
oneList [] _ = []
oneList (x : xs) (y : ys) = (x, y) : oneList xs ys

padRight :: Int -> String -> String -> String
padRight l padding str =
  if length str - l < 0
    then padRight l padding (str ++ padding)
    else str

myMaximumBy :: (Ord t1, Num t2, Num t1) => (t2 -> t1) -> [t2] -> t2
myMaximumBy = myMaximumBy' 0 0

myMaximumBy' :: Ord t1 => t2 -> t1 -> (t2 -> t1) -> [t2] -> t2
myMaximumBy' currMax _ _ [] = currMax
myMaximumBy' currMax currMaxVal f (a : as) =
  let newMaxVal = f a
   in if newMaxVal > currMaxVal
        then myMaximumBy' a newMaxVal f as
        else myMaximumBy' currMax currMaxVal f as

numOptions :: Integer -- keeping this integer so it can deal with lorge numbers
numOptions = product $ map (toInteger . length . snd) team

team :: Lineup
team = map (DB.second sort) squad

allTeams :: [Team]
allTeams = (sort . rmDups . concatMap snd) team

longestTeamName :: Team
longestTeamName = maximumBy (comparing length) allTeams

longestTeamNameLength :: Int
longestTeamNameLength = length longestTeamName

allOptions :: Lineup -> [[(Player, Team)]]
allOptions t = map (oneList (map fst t)) . mapM snd $ t

allOptionsProcessed :: Lineup -> [Option]
allOptionsProcessed =
  map
    ( map (\(players, team) -> (head team, players))
        . sortOn (Down . length . snd)
        . map unzip
        . groupBy (\(_, t1) (_, t2) -> t1 == t2)
        . sortOn snd
    )
    . allOptions

allOptionsProcessedPrinting :: Lineup -> [(IterationCount, Int, Option)]
allOptionsProcessedPrinting = allOptionsProcessedPrinting' 1 1 [] . allOptionsProcessed

allOptionsProcessedPrinting' :: IterationCount -> Int -> Option -> [Option] -> [Iteration]
allOptionsProcessedPrinting' _ _ _ [] = []
allOptionsProcessedPrinting' i n o (a : as) =
  let aLength = sum . map (length . snd) . take 3 $ a
   in if aLength > n
        then (i, aLength, a) : allOptionsProcessedPrinting' (i + 1) aLength a as
        else allOptionsProcessedPrinting' (i + 1) n o as

ppIteration :: Iteration -> String
ppIteration (iterationCount, maxValue, options) =
  intercalate "\n" $ ("Iteration " ++ show iterationCount) : map (("  " ++) . ppTeamPlayer) options
  where
    ppTeamPlayer (team, players) =
      padRight longestTeamNameLength " " team
        ++ " | "
        ++ (show . length) players
        ++ " | "
        ++ intercalate ", " players

main :: IO ()
main = putStrLn . intercalate "\n\n" . map ppIteration . allOptionsProcessedPrinting $ team
