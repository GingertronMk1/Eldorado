module Type where

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

