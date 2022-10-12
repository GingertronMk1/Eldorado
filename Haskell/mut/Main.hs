import Functions
import CalculatedData

main :: IO ()
main =
  putStrLn
  . ppOptions
  . foldFunction
  . map playerTeamToOption
  . lineupToPlayerTeams
  $ popSquad

