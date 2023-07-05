import System.Environment

main :: IO()
main = do
        val <- getArgs
        let actualVal = read (head val) :: Int
        putStrLn . show . isOdd $ actualVal

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = not . isOdd $ n - 1
