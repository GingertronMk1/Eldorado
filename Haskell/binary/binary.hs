{- |
   Module      : Main
   Copyright   : Nah
   License     : Nah
   Maintainer  : Jack Ellis
   Stability   : nah
   Portability : portable
-}

module Main where

-- |The 'main' function is just putting together everything else in the fuckin file tbh
main :: IO()
main = do (putStrLn . init . init . concat . map ((++", ") . show . binary)) url
          return ()

-- |'binary' takes a string representing a base-2 number and returns its value as a base-10 Integer
-- it more or less just takes a string, reverses it, and applies binary' to that with an initial
-- value of 0
binary :: String -> Integer
binary = binary' 0 . reverse

-- |'binary'' does the heavy lifting here; if it's at the end of the string
binary' :: Integer -> String -> Integer
binary' i (n:[]) = if n == '1' then 2^i else 0
binary' i ('0':ns) = binary' (i+1) ns
binary' i ('1':ns) = 2^i + binary' (i+1) ns

-- |'url' is just the test instance for the above
url :: [String]
url = ["01110100","01100101","01100011","01101000","00101101",
       "01110000","01101100","01100001","01100011","01100101",
       "01101101","01100101","01101110","01110100","01110011"]

