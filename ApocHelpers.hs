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
    | playerOf pieceFrom == player && (pieceFrom == WhitePawn || pieceFrom == BlackPawn) =
        isPawnMoveValid board player from to
    | playerOf pieceFrom == player && (pieceFrom == WhiteKnight || pieceFrom == BlackKnight) =
        isKnightMoveValid board player from to
    | otherwise = False
    where pieceFrom = pieceOf $ getFromBoard board from
          pieceTo = pieceOf $ getFromBoard board to
          getCellFrom = cell2Char $ getFromBoard board from

-- | Returns whether the pawn placement was a valid move
isValidPlacePawn :: Board -> (Int, Int) -> Bool
isValidPlacePawn board x = getFromBoard board x == E

-- | Determines the validity of a pawn movement
isPawnMoveValid :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isPawnMoveValid board player (fromX, fromY) to
       | (to == (fromX, fromY + forwards)) && (getCellTo == '_') = True
       | to == (fromX + 1, fromY + forwards) && playerOf pieceTo /= player = True
       | to == (fromX - 1, fromY + forwards) && playerOf pieceTo /= player = True
       | otherwise = False
       where forwards = case player of
               Black -> -1
               White -> 1
             pieceTo = pieceOf $ getFromBoard board to
             getCellTo = cell2Char $ getFromBoard board to

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
