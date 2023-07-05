import Data.List
import System.Environment (getArgs)

data PointsPurchase = PP {
  twoHundred :: Int,
  fiveHundred :: Int,
  oneThousandAndFifty :: Int,
  twoThousandEightHundred :: Int,
  fiveThousandEightHundredAndFifty :: Int,
  eightThousandNineHundred :: Int,
  twelveThousand :: Int,
  eighteenThousandFiveHundred :: Int
} deriving (Show)

total :: PointsPurchase -> Int
total pp = sum [
    200 * twoHundred pp,
    500 * fiveHundred pp,
    1050 * oneThousandAndFifty pp,
    2800 * twoThousandEightHundred pp,
    5850 * fiveThousandEightHundredAndFifty pp,
    8900 * eightThousandNineHundred pp,
    12000 * twelveThousand pp,
    18500 * eighteenThousandFiveHundred pp
  ]

ppPointsPurchase :: PointsPurchase -> String
ppPointsPurchase pp =
  unlines [
    "200 " ++ show (twoHundred pp),
    "500 " ++ show (fiveHundred pp),
    "1050 " ++ show (oneThousandAndFifty pp),
    "2800 " ++ show (twoThousandEightHundred pp),
    "5850 " ++ show (fiveThousandEightHundredAndFifty pp),
    "8900 " ++ show (eightThousandNineHundred pp),
    "12000 " ++ show (twelveThousand pp),
    "18500 " ++ show (eighteenThousandFiveHundred pp)
  ]

generate :: Int -> [(PointsPurchase, Int)]
generate n = [
  (pp, total pp) |
    twoHundred' <- [0..div n 200],
    fiveHundred' <- [0..div n 500],
    oneThousandAndFifty' <- [0..div n 1050],
    twoThousandEightHundred' <- [0..div n 2800],
    fiveThousandEightHundredAndFifty' <- [0..div n 5850],
    eightThousandNineHundred' <- [0..div n 8900],
    twelveThousand' <- [0..div n 12000],
    eighteenThousandFiveHundred' <- [0..div n 18500],
    let pp = PP {
    twoHundred = twoHundred',
    fiveHundred = fiveHundred',
    oneThousandAndFifty = oneThousandAndFifty',
    twoThousandEightHundred = twoThousandEightHundred',
    fiveThousandEightHundredAndFifty = fiveThousandEightHundredAndFifty',
    eightThousandNineHundred = eightThousandNineHundred',
    twelveThousand = twelveThousand',
    eighteenThousandFiveHundred = eighteenThousandFiveHundred'
  }
  ]

generateToNumber :: Int -> [(PointsPurchase, Int)]
generateToNumber n = filter ((==n) . snd) $ generate n

ppGenNumber :: Int -> String
ppGenNumber = intercalate "\n---\n" . map (ppPointsPurchase . fst) . generateToNumber

main :: IO ()
main = do
        args <- getArgs
        let [n] = args
        putStrLn . ppGenNumber $ (read n :: Int)