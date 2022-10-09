import Functions
import CalculatedData

main :: IO ()
main =
  putStrLn
  . ppOption
  . foldl (\o1 o2 -> if orderOptions o1 o2 == LT then o2 else o1) []
  . map playerTeamToOption
  $ allPTs
