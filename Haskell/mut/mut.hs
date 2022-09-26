import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Control.Parallel.Strategies

type Team = String
type Player = String
type Lineup = [(Player, [Team])]
type IterationNumber = Int
type Iteration = (TeamCount, [(Team, [Player])])
type TeamCount = Int

offense :: Lineup
offense = [
    ("Justin Herbert", ["Chargers"]),
    ("Justin Fields", ["Bears"]),
    ("Cordarrelle Patterson", ["Bears", "Falcons", "Raiders", "Patriots", "Vikings"]),
    ("Demarco Murray", ["Cowboys", "Eagles", "Titans", "Legends"]),
    ("Mark Ingram II", ["Saints", "Ravens", "Texans"]),
    ("Jim Taylor", ["Packers", "Saints", "Legends"]),
    ("Dave Casper", ["Raiders", "Titans", "Vikings", "Legends"]),
    ("Evan Engram", ["Giants", "Jaguars"]),
    ("Dawson Knox", ["Bills"]),
    ("DJ Moore", ["Panthers"]),
    ("Brandin Cooks", ["Patriots", "Rams", "Saints", "Texans"]),
    ("Devante Parker", ["Patriots", "Dolphins"]),
    ("Ceedee Lamb", ["Cowboys"]),
    ("Gabe Davis", ["Bills"]),
    ("Orlando Brown", ["Chiefs", "Ravens"]),
    ("Terron Armstead", ["Dolphins", "Saints"]),
    ("Isaac Seumalo", ["Eagles"]),
    ("Frank Ragnow", ["Lions"]),
    ("Tyler Shatley", ["Jaguars"]),
    ("Nate Davis", ["Titans"]),
    ("Lane Johnson", ["Eagles"])
  ]


defense :: Lineup
defense = [
    ("Zaven Collins", ["Cardinals"]),
    ("Jordyn Brooks", ["Seahawks"]),
    ("Divine Deablo", ["Raiders"]),
    ("Jalen Reeves-Maybin", ["Lions", "Texans"]),
    ("Harrison Smith", ["Vikings"]),
    ("Grant Delpit", ["Browns"]),
    ("Trevon Moehrig", ["Raiders"]),
    ("Budda Baker", ["Cardinals"]),
    ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
    ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
    ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"]),
    ("Mike Hilton", ["Bengals", "Steelers"]),
    ("Randy Moss", ["Raiders", "Patriots", "Titans", "Legends", "Vikings", "49ers"]),
    ("Sam Adams", ["CAPTAIN"]),
    ("Deforest Buckner", ["49ers", "Colts"]),
    ("Derrick Brown", ["Panthers"])
  ]

specialTeams :: Lineup
specialTeams = []

squad :: Lineup
squad = offense ++ defense ++ specialTeams

rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

oneList :: [a] -> [b] -> [(a, b)]
oneList _ [] = []
oneList [] _ = []
oneList (x : xs) (y : ys) = (x, y) : oneList xs ys

padRight :: Int -> String -> String -> String
padRight l padding str = if length str < l
                         then padRight l padding (str ++ padding)
                         else str

numOptions :: Integer   -- Keeping this integer so it can deal with lorge numbers
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
allOptions = map (oneList (map fst team)) $ mapM snd

allOptionsProcessed :: Lineup -> [[[(Player, Team)]]]
allOptionsProcessed = map (groupBy (\(_, t1) (_, t2) -> t1 == t2) . sortOn snd) allOptions

allOptionsProcessedGrouped :: Lineup -> [[([Player], [Team])]]
allOptionsProcessedGrouped = map (sortOn (Down . length . snd) . map unzip) allOptionsProcessed

allOptionsFullyProcessed :: Lineup [[(Team, [Player])]]
allOptionsFullyProcessed = map (map (\(players, team) -> (head team, players))) allOptionsProcessedGrouped

allOptionsSortedSieved :: Lineup -> [Iteration]
allOptionsSortedSieved = allOptionsSortedSieved' allOptionsFullyProcessed

allOptionsSortedSieved' :: Lineup -> [[(Team, [Player])]] -> [Iteration]
allOptionsSortedSieved' = map allOptionsSortedSieved''

allOptionsSortedSieved'' :: Lineup -> [(Team, [Player])] -> Iteration
allOptionsSortedSieved'' a = (length . snd . head $ a, a)

parAllOptions = parMap rpar allOptionsSortedSieved'' allOptionsFullyProcessed

parAllOptions' = parMap rpar allOptionsSortedSieved''

ppAllOptions :: IO ()
ppAllOptions = ppAllOptions' allOptionsSortedSieved

ppAllOptions' :: [(Int, [(String, [String])])] -> IO ()
ppAllOptions' =
  putStrLn
    . intercalate "\n\n"
    . map (\(iteration, teamChems) ->
        "Iteration "
        ++ show iteration
        ++ "/"
        ++ show numOptions
        ++ " ("
        ++ (show . (100*) . div (toInteger iteration)) numOptions
        ++ "%)"
        ++ "\n"
        ++ intercalate "\n" (map (\(team, players) -> padRight (longestTeamNameLength + 1) " " team ++ " | " ++ show (length players) ++ " | " ++ intercalate ", " players) teamChems))

main :: IO ()
-- main = ppAllOptions
-- main = print . maximumBy (comparing fst) $ allOptionsSortedSieved
main = print $ maximumBy (comparing fst) $ parAllOptions offense
