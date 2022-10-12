module CalculatedData where

import Data
import Type
import Functions

import Data.Bifunctor as DB
import Data.List


squad :: Lineup
squad =
  popularitySort
  $ concatMap
    (concatMap snd)
    [ offense,
      defense,
      specialTeams
    ]

numberOfEachTeam :: [(Team, Int)]
numberOfEachTeam = numberOfEachTeam' squad

team :: Lineup
team = map (DB.second sort) squad

allPTs :: [[(Player, Team)]]
allPTs = lineupToPlayerTeams squad

firstPT :: [(Player, Team)]
firstPT = head allPTs