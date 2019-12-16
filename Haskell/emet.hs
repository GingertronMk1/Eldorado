main :: IO()
--main = putStrLn . concat . map ((++"\n") . show . emet'') . words $ testString
main = putStr . concat . map (++"\n") . emet $ testString
--main = putStrLn . (\(s,e) -> s ++ e) . organiser $ testString

emet :: String -> [String]
emet w = let (first, second) = emet' ([],[]) . words $ w
         in (reverse first) ++ second

emet' :: ([String], [String]) -> [String] -> ([String], [String])
emet' (firsts, seconds) (s:[]) = let (f', s') = emet'' s
                                 in (f' : firsts, s' : seconds)
emet' (firsts, seconds) (s:ss) = case s of "+" -> emet' ((head seconds):firsts, tail seconds) ss
                                           _   -> let (f', s') = emet'' s
                                                  in emet' (f' : firsts, s' : seconds) ss

emet'' :: String -> (String, String)
emet'' = organiser

testString :: String
testString = ".container.row.column#somethingelse p + .container-fluid"

organiser :: String -> (String, String)
organiser s = let (e',i',c') = organiser' s
                  (e,i,c) = (dropWhile (==' ') e', dropWhile (==' ') i', dropWhile (==' ') c')
                  element   = e
                  identity  = if i == "" then "" else " id=\"" ++ i ++ "\""
                  className = if c == "" then "" else " class=\"" ++ c ++ "\""
               in ("<" ++ element ++ identity ++ className ++ ">", "</" ++ element ++ ">")

organiser' :: String -> (String, String, String)
organiser' s = let (e', i', c') = organiser'' ("","","") s 'n'
                in if elem (head s) ".#" then ("div", reverse i', reverse c')
                                         else (reverse e', reverse i', reverse c')

organiser'' :: (String, String, String) -> String -> Char -> (String, String, String)
organiser'' t [] _ = t
organiser'' t@(e,i,c) (s:ss) m = case s of '.' -> organiser'' (e,i,' ':c) ss 'c'
                                           '#' -> organiser'' (e,' ':i,c) ss 'i'
                                           _   -> case m of 'c' -> organiser'' (e,i,s:c) ss m
                                                            'i' -> organiser'' (e,s:i,c) ss m
                                                            'n' -> organiser'' (s:e,i,c) ss m
