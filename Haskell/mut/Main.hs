import Type
import Functions
import CalculatedData

foldFunction :: [Option] -> [Option]
foldFunction [] = []
foldFunction os = foldFunction' os []

foldFunction' :: [Option] -> Option -> [Option]
foldFunction' [] _ = []
foldFunction' (o:os) biggestO =
  if orderOptions o biggestO == GT
    then o:foldFunction' os o
    else foldFunction' os biggestO

main :: IO ()
main =
  putStrLn
  . ppOptions
  . foldFunction
  . map playerTeamToOption
  $ allPTs
