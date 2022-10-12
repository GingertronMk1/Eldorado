import Functions
import CalculatedData

main :: IO ()
main =
  putStrLn
  . ppOptions
  . foldFunction
  . allUsefulOptions
  . popFilter
  $ squad

