primes :: [Integer]
primes = 2 : 3 : sieve [5, 7..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x^2 `mod` p > 0]

primeFactorsOf :: Integer -> [(Integer, Integer)]
primeFactorsOf n
    | n <= 3 = [(n, 1)]
    | otherwise = filter (\x -> snd x > 0) (zip (relevantPrimes) (map (dividesXTimes n) (relevantPrimes))) 
        where 
           dividesXTimes n x
              | n `mod` x == 0 = 1 + dividesXTimes (n `div` x) x
              | otherwise = 0

           relevantPrimes = takeWhile (<n) primes

main = do
        print $ primeFactorsOf 600851475143
