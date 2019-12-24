main :: IO()
main = ppHTML . emet $ testString

ppHTML :: [String] -> IO()
ppHTML = putStr . concat . map (++"\n") . ppHTML' 0

ppHTML' :: Int -> [String] -> [String]
ppHTML' _ []     = []
ppHTML' n (s:ss) = if take 2 s == "</" then let n' = n-2
                                             in (replicate n' ' ' ++ s) : ppHTML' n' ss
                                       else (replicate n ' ' ++ s) : ppHTML' (n+2) ss

emet :: String -> [String]
emet w = let (first, second) = emet' ([],[]) . words $ w
         in first ++ second

emet' :: ([String], [String]) -> [String] -> ([String], [String])
emet' (firsts, seconds) (s:[]) = let (f', s') = emet'' s
                                  in (reverse (f' : firsts), s' : seconds)
emet' (firsts, seconds) (s:ss) = if s == "+" then emet' (head seconds : firsts, tail seconds) ss
                                             else let (f', s') = emet'' s
                                                   in emet' (f' : firsts, s' : seconds) ss

emet'' :: String -> (String, String)
emet'' = organiser

testString :: String
testString = ".container.row.column#somethingelse p + .container-fluid span#test-span"

organiser :: String -> (String, String)
organiser s = let (e',i',c') = organiser' s
                  (e,i,c) = (dropWhile (==' ') e', dropWhile (==' ') i', dropWhile (==' ') c')
                  element   = if e /= "" then e
                                         else "div"
                  identity  = if i /= "" then " id=\"" ++ i ++ "\""
                                         else ""
                  className = if c /= "" then " class=\"" ++ c ++ "\""
                                         else ""
               in ("<" ++ element ++ identity ++ className ++ ">", "</" ++ element ++ ">")

organiser' :: String -> (String, String, String)
organiser' s = let (e', i', c') = organiser'' ("","","") s 'n'
                in (reverse e', reverse i', reverse c')

organiser'' :: (String, String, String) -> String -> Char -> (String, String, String)
organiser'' t [] _ = t
organiser'' t@(e,i,c) (s:ss) m = case s of '.' -> organiser'' (e,i,' ':c) ss 'c'
                                           '#' -> organiser'' (e,' ':i,c) ss 'i'
                                           _   -> case m of 'c' -> organiser'' (e,i,s:c) ss m
                                                            'i' -> organiser'' (e,s:i,c) ss m
                                                            'n' -> organiser'' (s:e,i,c) ss m
