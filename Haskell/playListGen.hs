-- IMPORTING THINGS

import Data.List                      (sort, sortBy, intersperse, isPrefixOf, isSuffixOf)
import Data.List.Split                (splitOn)
import Data.Ord                       (comparing)
import System.Directory               (getDirectoryContents)
import qualified Data.Text as T       (unpack)
import qualified Data.Text.IO as TIO  (readFile)

-- TYPES AND VARIABLES

baseMusicFolder :: FilePath
baseMusicFolder = "/Users/Jack/Music/Music/"
musicFolder :: FilePath
musicFolder = baseMusicFolder ++ "Albums/"
playlistFolder :: FilePath
playlistFolder = baseMusicFolder ++ "Playlists/"
playlistFile :: FilePath
playlistFile = "/Users/Jack/Documents/Playlists.txt"
playlists :: IO [(String, [String])]
playlists = (fmap T.unpack . TIO.readFile) playlistFile >>= return . map (\(n, ss) -> (n, map (swapFirst " - " "/") ss)) . listToTuples . splitOn [""] . lines

-- HELPERS

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

swapFirst :: Eq a => [a] -> [a] -> [a] -> [a]
swapFirst swapOut swapIn = flatten . intersperse swapIn . (\(a:as) -> [a] ++ [((flatten . intersperse swapOut) as)]) . splitOn swapOut

listToTuples :: [[String]] -> [(String, [String])]
listToTuples [] = []
listToTuples (a:as) = (head a, drop 2 a):(listToTuples as)

baseName :: FilePath -> String
baseName = last . splitOn "/"

-- FUN BIT

playlistGen :: (String, [String]) -> IO (String, [FilePath])
playlistGen (t, ss) = (sequence . map playlistGen') ss >>= \fps -> return (t, (flatten) fps)

playlistGen' :: String -> IO [FilePath]
playlistGen' s = getDirectoryContents (musicFolder++s) >>= return . sort . map (\fp -> "../Albums/"++s++"/"++fp) . filter (not . isPrefixOf ".")

sortPlaylist :: (String, [FilePath]) -> (String, [FilePath])
sortPlaylist (s, fps)
  | sortFn == "songName"  = (playlistName, sortBy (comparing (dropWhile (/=' ') . baseName)) fps)
  | sortFn == "normal"    = (playlistName, fps)
  where splitHeading = splitOn " : " s
        playlistName = (flatten . init) splitHeading
        sortFn       = last splitHeading

m3uGen :: (String, [FilePath]) -> (String, String)
m3uGen (t, fps) = (playlistFolder++t++".m3u",  (flatten . intersperse "\n" . filter (isSuffixOf ".flac")) fps)

createPlaylist :: (String, [FilePath]) -> IO (String, String)
createPlaylist (t, ss) = playlistGen (t, ss) >>= return . m3uGen . sortPlaylist

writePlaylist :: (String, [FilePath]) -> IO ()
writePlaylist (t, ss) = createPlaylist (t, ss) >>= \(t2, ss2) -> writeFile t2 ss2
--writePlaylist (t, ss) = createPlaylist (t, ss) >>= \(t2, ss2) -> putStrLn (t2 ++ "\n" ++ replicate (length t2) '-' ++ "\n" ++ ss2)

main = do pl <- playlists
          (sequence . map writePlaylist) pl
