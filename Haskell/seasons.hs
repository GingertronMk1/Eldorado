import Data.List          (sort, sortBy, group, groupBy, intersperse)
import Data.Ord           (comparing)
import Prelude    hiding  (all)

rmDups :: Ord a => [a] -> [a]
rmDups = map head . group . sort

seasonShows :: [(String, [Int])]
seasonShows = let all = [1..7] :: [Int]
               in [
                   ("Eve",[7])
                  ,("Forget",[6,7])
                  ,("Here",[7,2,5,1])
                  ,("McPlay",[4,5,3])
                  ,("Rat",[7,5])
                  ,("Vehement",[6,5,2,1])
                  ,("Watch",[6,3,7])
                  ]

seasonGen :: [(String, [Int])] -> [[(String, Int)]]
seasonGen = filter (not . duplicates . map fst)
            . sequence
            . groupBy (\(_,as) (_,bs) -> as==bs)
            . sortBy (comparing snd)
            . concat
            . map (\(a,bs) -> [(a,b) | b<-bs])

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (a:as) = if elem a as then True else duplicates as

seasons' :: [[(String, Int)]] -> IO ()
seasons' [] = putStrLn "No shows provided"
seasons' a  = let ca = concat a
                  longestName = ((+1) . length . last . sortBy (comparing length) . map fst) ca
                  addPipe     = concat . intersperse "| "
                  padding x   = replicate (longestName - (length x)) ' '
                  headerRow   = addPipe [let n' = "Slot " ++ show n in n' ++ padding n' | n<-(rmDups . map snd) ca]
                  ppShows     = (concat . intersperse "\n" . map (\ss -> addPipe [n ++ padding n | n<-(map fst) ss])) a
               in putStrLn (headerRow ++ "\n" ++ replicate (length headerRow) '-' ++ "\n" ++ ppShows)

count :: Ord a => [a] -> [(a, Int)]
count = map (\l -> (head l, length l)) . group . sort

main :: IO ()
main = seasons' . seasonGen $ seasonShows
