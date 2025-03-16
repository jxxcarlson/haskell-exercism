module Prime ( nth, isPrime) where

factors num = 
    factors' num 2
    where 
        factors' num fact
          | num == 1 = []
          | (num `rem` fact == 0) = fact : factors' (num `div` fact) fact
          | otherwise = factors' num (fact + 1)

isPrime num = 
    isPrime' num 2
    where 
        isPrime' num fact
          | num == 1 = False
          | fact * fact  > num = True
          | (num `rem` fact == 0) = False
          | fact == 2 = isPrime' num (fact + 1)
          | otherwise = isPrime' num (fact + 2)
         


-- isPrime :: Integer -> Bool
-- isPrime n = length (factors n) == 1

accumulatePrimes count  = 
    accumulatePrimes' count 2 []
    where 
        accumulatePrimes' count currentNumber list
            | length list >= count = list 
            | isPrime currentNumber = accumulatePrimes' count (currentNumber + 1) (list ++ [currentNumber])
            | otherwise = accumulatePrimes' count (currentNumber + 1) list
              
{-
  Compute the nth prime number.
-}
nth_ :: Int -> Maybe Integer
nth_  n = 
  let primes = accumulatePrimes n 
  in
    if length primes == n then Just (head $ reverse $ primes) else Nothing -- "You need to implement this function."


nth n = 
   if n < 1 then Nothing
   else nth_ n