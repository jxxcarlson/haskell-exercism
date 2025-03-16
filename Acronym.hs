module Acronym (abbreviate, t1, t2, t3, replaceCharacter) where

import Data.Char (toUpper, isPunctuation)

--abbreviate :: String -> String

abbreviate  = foldl (++) [] . map (deleteLowerCase . capitalize . fixAllCaps . removePunctuation ) . (words . replaceCharacter '-' ' ')

removePunctuation :: String -> String
removePunctuation xs = filter (not . isPunctuation) xs

replaceCharacter :: Char -> Char -> String -> String
replaceCharacter old new xs = map (\x -> if x == old then new else x) xs


-- abbreviate :: String -> String
deleteLowerCase :: String -> String
deleteLowerCase xs = filter isUpper xs

capitalize :: String -> String
capitalize xs = 
    case xs of 
        [] -> []
        (x:xs) -> toUpper x : xs

isAllCaps :: String -> Bool
isAllCaps xs = all isUpper xs

fixAllCaps :: String -> String
fixAllCaps xs = if isAllCaps xs then head xs : [] else xs

isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'


t1 = "GNU Image Manipulation Program"

t2 = "First In, First Out"

t3 = "Complementary metal-oxide semiconductor"