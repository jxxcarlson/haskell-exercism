module Queens (boardString, canAttack) where

import Data.List (intercalate)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
     (intercalate "\n" $ map (\r -> makeBoardRow r white black) [0..7]) ++ "\n"
  
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (qaR, qaC) (qbR, qbC) = 
    qaR == qbR || qaC == qbC || abs (qaR - qbR) == abs (qaC - qbC)


printBoard :: Maybe (Int, Int) -> Maybe (Int, Int) -> IO ()
printBoard white black = 
    putStrLn $ makeBoard white black


makeBoard :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
makeBoard white black = 
    intercalate "\n" $ map (\r -> makeBoardRow r white black) [0..7]
   
border = "  " ++ (intercalate " " $ map (:[]) $ "abcdefgh") ++ "  "

makeBoardRow :: Int -> Maybe (Int, Int) -> Maybe (Int, Int) -> String
makeBoardRow r white black = 
    intercalate " " $ 
      map (:[]) $ 
      placeQueen r white 'W' $ 
      placeQueen r black 'B' $  
      replicate 8 '_' 

prefix :: Int -> String -> String
prefix k s = show (7 - k) ++ " " ++ s

suffix :: Int -> String -> String
suffix k s = s ++ " " ++ show (7 - k)

changeElement :: Int -> a -> [a] -> [a]
changeElement k newVal xs = [if i == k then newVal else x | (x,i) <- zip xs [0..]]

placeQueen  :: Int -> Maybe (Int, Int) -> Char -> String -> String
placeQueen  row position color boardRow = 
    case position of
        Nothing -> boardRow
        Just (qRow, qColumn) -> 
            if row == qRow then changeElement qColumn color boardRow else boardRow

