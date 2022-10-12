module CalculatedData where

import Data
import Type
import Functions

squad :: Lineup
squad =
  popularitySort
  $ concatMap
    (concatMap snd)
    [ offense,
      defense,
      specialTeams
    ]

popSquad :: Lineup
popSquad = popFilter squad
