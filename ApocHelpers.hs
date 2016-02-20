module ApocHelpers where


import Data.List
import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.Exit
import ApocTools

-- | Determines all the valid moves for the pieces on the board
isValidPlay :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isValidPlay board player from to
    | getCellFrom == '_' = False
    | playerOf pieceFrom == player && (pieceFromCell == WP || pieceFromCell == BP) =
        isPawnMoveValid board player from to
    | playerOf pieceFrom == player && (pieceFromCell == WK || pieceFromCell == BK) =
        isKnightMoveValid board player from to
    | otherwise = False
    where pieceFrom = pieceOf $ getFromBoard board from
          getCellFrom = cell2Char $ getFromBoard board from
          pieceFromCell = getFromBoard board from

-- | Returns whether the pawn placement was a valid move
isValidPlacePawn :: Board -> (Int, Int) -> Bool
isValidPlacePawn board x = getFromBoard board x == E

-- | Determines the validity of a pawn movement
isPawnMoveValid :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isPawnMoveValid board player (fromX, fromY) to
       | (to == (fromX, fromY + forwards)) && (getCellTo == '_') = True
       | to == (fromX + 1, fromY + forwards) && (notSamePlayer) = True
       | to == (fromX - 1, fromY + forwards) && (notSamePlayer) = True
       | otherwise = False
       where forwards = case player of
               Black -> -1
               White -> 1
             getCellTo = cell2Char $ getFromBoard board to
             pieceToCell = getFromBoard board to
             notSamePlayer = if (player == Black) 
                            then ((pieceToCell == WK) || (pieceToCell == WP)) 
                            else ((pieceToCell == BK) || (pieceToCell == BP))

-- | Determines the validity of a knight movement
isKnightMoveValid :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isKnightMoveValid board player (fromX, fromY) to
         | ((to == (fromX + 1, fromY + 2)) && checked) = True
         | ((to == (fromX + 1, fromY - 2)) && checked) = True
         | ((to == (fromX - 1, fromY + 2)) && checked) = True
         | ((to == (fromX - 1, fromY - 2)) && checked) = True
         | ((to == (fromX + 2, fromY + 1)) && checked) = True
         | ((to == (fromX + 2, fromY - 1)) && checked) = True
         | ((to == (fromX - 2, fromY + 1)) && checked) = True
         | ((to == (fromX - 2, fromY - 1)) && checked) = True
         | otherwise = False
         where pieceAtTo = playerOf $ pieceOf $ getFromBoard board to
               getCellTo = getFromBoard board to
               checked = ((getCellTo == E) || (pieceAtTo /= player))

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
   in (if null zs then (if null ys then [] else init ys) else ys)
       ++ [elem]
       ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2 :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- | Counts the occurences of an element in a list
count :: (Eq a) => [a] -> a -> Int
count xs x = length $ filter (== x) xs

-- | Counts the occurences of an element in a list of lists
count2 :: (Eq a) => [[a]] -> a -> Int
count2 xs x = sum $ map (\xs -> count xs x) xs
