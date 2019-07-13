granddads :: [String]
granddads = ["Ronald", "Terry"]

streets :: [String]
streets = ["Wilbert", "Victoria", "Mace", "Poplars"]

headteachers :: [String]
headteachers = ["Hyland", "Goodwin", "Hodson"]

nameGen :: [String]
nameGen = [x ++ " " ++ y ++ "-" ++ z | x <- granddads, y <- streets, z <- headteachers]

main :: IO ()
main = putStr . concat . map ((++"\n") . show) $ nameGen
