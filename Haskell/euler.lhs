> import Data.List  (group, sort, sortBy)
> import Data.Ord   (comparing)


Problem 1: multiples of 3 and 5

> problem1 = sum [n | n <- [0..999], mod n 3 == 0 || mod n 5 == 0]

Problem 2: Even Fibonacci Numbers

> fibs :: [Int]
> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

> fibsTo :: Int -> [Int]
> fibsTo n = takeWhile (<=n) fibs

> problem2 = (sum . filter (\n -> mod n 2 == 0) . fibsTo) 4000000

Problem 3: Largest prime factor

> factorsOf :: Int -> [Int]
> factorsOf n = [f | f <- [1..(ceiling . sqrt . fromIntegral) n], mod n f == 0]

> isPrime :: Int -> Bool
> isPrime n = factorsOf n == [1]

> primesTo :: Int -> [Int]
> primesTo n = [x | x <- [2..n], isPrime x]

> primeFactors :: Int -> [Int]
> primeFactors n = case factors of [] -> [n]
>                                  _  -> factors ++ primeFactors (div n (head factors))
>                  where factors = take 1 [x | x <- factorsOf n, x /= 1 && x /= n]

> problem3 = (maximum . primeFactors) 600851475143

Problem 4: Largest palindrome product

> isPalindrome :: Int -> Bool
> isPalindrome = isPalindrome' . show

> isPalindrome' :: String -> Bool
> isPalindrome' n = let ln = length n
>                   in case mod ln 2 of 0 -> let (f, r) = splitAt (div ln 2) n
>                                             in f == reverse r
>                                       1 -> let (f, x:r) = splitAt (div ln 2) n
>                                             in f == reverse r

> problem4 = let ns = [100..999]
>             in (maximum . filter isPalindrome) [n1 * n2 | n1 <- ns, n2 <- ns]

Problem 5: Smallest Multiple

> divisibleBy :: Int -> Int -> Bool -- N is divisible by d
> divisibleBy n d = mod n d == 0

> divisibleByTo :: Int -> Int -> Bool
> divisibleByTo d n = divisibleByTo' n [2..d]

> divisibleByTo' _ [] = True
> divisibleByTo' n (c:cs) = if divisibleBy n c then divisibleByTo' n cs
>                                              else False

> problem5 1 = 1
> problem5 n = (head . filter (divisibleByTo n)) [1..]

Problem 6: Sum square difference

> sumSquareDiff n = let toN = [1..n]
>                       sumSquare = (sum . map (^2)) toN
>                       squareSum = ((^2) . sum) toN
>                   in abs $ sumSquare - squareSum

> problem6 = sumSquareDiff 100

Problem 7: The 10001st Prime

> nthPrime :: Int -> Int
> nthPrime n = ((!!(n-1)) . filter (isPrime)) [1..]

> problem7 = nthPrime 10001

I'm not doing problem 8, that number's waaay too big

Problem 9: Special Pythagorean Triplet

> isPyTrip :: Int -> Int -> Int -> Bool
> isPyTrip a b c = a < b && b < c && a^2 + b^2 == c^2

> problem9 = let m = 500
>             in head [a*b*c | a <- [1..m]
>                            , b <- [a..m]
>                            , c <- [b..m]
>                            , isPyTrip a b c
>                            , a + b + c == 1000]

Problem 10: Summation of primes

> sumPrimes :: Int -> Int
> sumPrimes = sum . primesTo

> problem10 = sumPrimes 2000000

Not doing problem 11, more big things to copy-paste

> triangle :: Int -> Int
> triangle n = sum [1..n]

> divisors :: Int -> [Int]
> divisors n = filter (\d -> mod n d == 0) [1..n]

> problem12 = (head . filter (\n -> length (divisors n) > 500) . map triangle) [1..]

Problem 14: Longest Collatz sequence

> collatz' :: [Int] -> [Int]
> collatz' ns = let l = last ns
>               in if l == 1 then ns
>                  else case mod l 2 of 0 -> collatz' (ns ++ [div l 2])
>                                       1 -> collatz' (ns ++ [(3*l) + 1])

> collatz :: Int -> [Int]
> collatz n = collatz' [n]

> problem14 = (last . sortBy (comparing length) . map collatz) [1..999999]

Problem 16: Power digit sum

> digitSum :: Int -> Int
> digitSum = sum . map (\c -> read [c] :: Int) . show


Problem 20: Factorial digit sum


Problem 21: Amicable Numbers

> divisors2 :: Int -> [Int]
> divisors2 = init . divisors

> amicable :: Int -> Int -> Bool
> amicable a b = a /= b && sum (divisors2 a) == b && sum (divisors2 b) == a

> problem21 = let ns = [1..9999]
>             in concat [[a,b] | a <- ns, b <- [a, a-1..1], amicable a b]

Problem 25: 1000-digit fib

> noDigits :: Int -> Int
> noDigits = length . show

> problem25 = (length . takeWhile (\n -> noDigits n <= 1000)) fibs

Problem 29: Distinct Powers

> rmDups :: (Ord a) => [a] -> [a]
> rmDups = map head . group . sort

> sorted [n] = True
> sorted (n1:n2:ns) = if n1 == n2 then False
>                                 else sorted (n2:ns)

> problem29 = (length .rmDups) [a^b | a <- [2..100], b <- [2..100]]

Problem 30: Digit 5th Powers

> digits :: Int -> [Int]
> digits = map (\c -> read [c] :: Int) . show

> sumOfFifths :: Int -> Bool
> sumOfFifths n = ((n==) . sum . map (^5) . digits) n

> problem30 = [n | n <- [2..1000000], sumOfFifths n]

Problem 31: Coin sums

> problem31 = length [show p1 ++ " x 1p, " ++ show p2 ++ " x 2p, " ++ show p5 ++ " x 5p, " ++ show p10 ++ " x 10p, " ++ show p20 ++ " x 20p, " ++ show p50 ++ " x 50p, " ++ show pound ++ " x £1, " ++ show pound2 ++ " x £2\n"
>                       | p1      <- [0..200]
>                       , p2      <- [0..100]
>                       , p5      <- [0..40]
>                       , p10     <- [0..20]
>                       , p20     <- [0..10]
>                       , p50     <- [0..4]
>                       , pound   <- [0..2]
>                       , pound2  <- [0..1]
>                       , p1 + (2*p2) + (5*p5) + (10*p10) + (20*p20) + (50*p50) + (100*pound) + (200*pound2) == 200]


