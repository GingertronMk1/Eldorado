module CalculatedData where

import Data
import Type
import Functions

import Data.Bifunctor as DB
import Data.List


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

numOptions :: Int
numOptions = product $ map (length . snd) team

team :: Lineup
team = map (DB.second sort) squad

allTeams :: [Team]
allTeams = (sort . rmDups . concatMap snd) team

allPTs :: [[(Player, Team)]]
allPTs = allOptionsAlt squad

firstPT :: [(Player, Team)]
firstPT = head allPTs

allOptionsAlt :: Lineup -> [[(Player, Team)]]
allOptionsAlt = mapM (\(p, ts) -> [(p, t) | t <- ts])

ppPTToOptions :: IO()
ppPTToOptions =
  putStrLn
  . ppOption
  . foldl (\o1 o2 -> if orderOptions o1 o2 == LT then o2 else o1) []
  . map playerTeamToOption
  $ allPTs