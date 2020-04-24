module Primes (primes, test) where

primes :: Integer -> Maybe [Integer]
primes n
 | n < 0 = Nothing
 | otherwise = thenes [1..n] n 2

thenes :: [Integer] -> Integer -> Integer -> Maybe [Integer]
thenes l n m 
 | n <= 0 || m <= 0 = Nothing
 | otherwise = if (m>=n) then Just l else thenes (rm l m) n (m+1)

rm :: [Integer] -> Integer -> [Integer]
rm [] m = []
rm (h:t) m = if (h /= m && rem h m == 0) then rm t m else h:(rm t m)

test :: Bool
test = primes 1 == Just [1]
 && primes 2 == Just [1,2]
 && primes 3 == Just [1,2,3]
 && primes 4 == Just [1,2,3]
 && primes 5 == Just [1,2,3,5]
 && primes 6 == Just [1,2,3,5]
 && primes 7 == Just [1,2,3,5,7]
 && primes 11 == Just [1,2,3,5,7,11]
 && primes 0 == Nothing
 && primes (-1) == Nothing
