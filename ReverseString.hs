module ReverseString (reverseString, reverseString') where


-- O(n^2 )
reverseString' :: String -> String
reverseString' [] = []
reverseString' (x:xs) = reverseString' xs ++ [x]

-- O(n)
reverseString :: [a] -> [a]
reverseString str = accumulate str []
  where
    accumulate [] acc = acc
    accumulate (x:xs) acc = accumulate xs (x:acc)


