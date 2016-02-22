module ApocAIHelper where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import ApocHelpers
import System.Random

-- | This type is used to define moves, primarily for ease of typing
type Move = ((Int, Int), (Int, Int))
-- | SortedMoves is a data structure that allows us to sort the types of moves very easily
data SortedMoves = SortedMoves {
                    emptyMoves  :: [Move], -- ^ Any moves where the piece will move to an empty position
                    pawnMoves   :: [Move], -- ^ Any move where the piece will move to an enemy pawn
                    knightMoves :: [Move]} -- ^ Any move where the piece will move to an enemey knight


-- | Looks for the first empty position to place its new pawn. Works like a double-for loop in java
validPawnPlacement :: Int -- ^ x-coordinate on the board.
                      -> Int  -- ^ y-coordiante on the board.
                      -> Board -- ^ the board
                      -> Player -- ^ the player
                      -> (Int, Int) -- ^ the tuple to return
validPawnPlacement x 5 board player =  validPawnPlacement x 0 board player -- ^ bound case. After 5, it goes and checks the next row
validPawnPlacement x y board player = if (getFromBoard board (x,y) == E)  -- ^ checks if empty, if so places pawn, otherwise recurses.
                                      then (x,y)
                                      else validPawnPlacement x (y+1) board player

-- | Helper function. Given an index and a SortedMoves list, returns the list of moves that capture that piece (knight, pawn, empty)
getSortedMovesbyInt :: SortedMoves -> Int -> [Move]
getSortedMovesbyInt moves index
    | index == 0 = emptyMoves moves
    | index == 1 = pawnMoves moves
    | index == 2 = knightMoves moves
    | otherwise = []

-- | Given a list, will return an element from the list
pickRandom ::  [a] -> IO a
pickRandom list = do
  index <- randomRIO (0, (length list)-1) -- ^ Generates a number between zero and the length of the list
  return (list !! index)

-- | Given the board and the moves will sort the moves possible into possible moves
sortMoves :: Board -> [Move] -> SortedMoves
sortMoves _ [] = SortedMoves [] [] [] -- ^ Stop pattern when no more moves possible
sortMoves board (x:xs) =
  let moves = sortMoves board xs -- ^ Recurse until empty
  in case (getFromBoard board (snd x)) of
    -- | Sorts based on all possible cases
    E ->  SortedMoves (x:(emptyMoves moves)) (pawnMoves moves) (knightMoves moves)
    BP -> SortedMoves (emptyMoves moves) (x:(pawnMoves moves)) (knightMoves moves)
    WP -> SortedMoves (emptyMoves moves) (x:(pawnMoves moves)) (knightMoves moves)
    BK -> SortedMoves (emptyMoves moves) (pawnMoves moves) (x:(knightMoves moves))
    WK -> SortedMoves (emptyMoves moves) (pawnMoves moves) (x:(knightMoves moves))
-- | Given the board and player, returns ALL the possible moves that player can make
getAllPossibleMoves :: Board -> Player -> [Move]
getAllPossibleMoves board player =
  let pieces = getAllPieces board player -- ^ Gets all the pieces, and moves
      moves = getMovesFromPieces board pieces
      validMoves = filter (\x -> isValidPlay board player (fst x) (snd x)) moves -- ^ filters those moves by checking if they are valid moves,  then returning the new list
  in validMoves

-- | Returns all the pieces on the board for the player
getAllPieces :: Board -> Player -> [(Int, Int)]
getAllPieces board Black = getInGrid (\x -> x == BK || x == BP) board 0
getAllPieces board White = getInGrid (\x -> x == WK || x == WP) board 0

-- | Helper function, returns type of piece looking for
getInGrid :: (a -> Bool) -> [[a]] -> Int -> [(Int, Int)]
getInGrid fn [] _ = []
getInGrid fn (x:xs) row = (map (\x -> (x, row)) $ getInRow fn x 0) ++ (getInGrid fn xs (row+1))
-- | Helper function, returns type of pieces looking for for only a specific row
getInRow :: (a -> Bool) -> [a] -> Int -> [Int]
getInRow fn [] _ = []
getInRow fn (x:xs) col = if fn x then col:(getInRow fn xs (col+1)) else getInRow fn xs (col+1)
-- | Given a piece, return move. Created to help get all moves possible
getMovesFromPieces :: Board ->  [(Int, Int)] -> [Move]
getMovesFromPieces _ [] = []
getMovesFromPieces board (x:xs) = let piece = filter ((rangeChecker).fst) (filter ((rangeChecker).snd) (getMovesFromSinglePiece board x)) in
                                  (createMoveFromSinglePiece x piece)++(getMovesFromPieces board xs)
-- | Helper function, design to check if int is in range.
rangeChecker :: Int -> Bool
rangeChecker x = if (x < 5 && x > -1) then True else False
-- | Given a piece (x,y) and the location it can move it generates the moves the piece can make in the [Move] format
createMoveFromSinglePiece :: (Int, Int) -> [(Int, Int)] -> [Move]
createMoveFromSinglePiece _ [] = []
createMoveFromSinglePiece src (x:xs) = (src, x):(createMoveFromSinglePiece src xs)
-- | This will return the moves a piece is able to make with no regard to error checking. This is a very static "helper" function
getMovesFromSinglePiece :: Board -> (Int, Int) -> [(Int, Int)]
getMovesFromSinglePiece board (srcX, srcY) = let piece = (srcX, srcY) in case (getFromBoard board (srcX, srcY)) of
                BP -> [(srcX, srcY-1),(srcX-1, srcY-1), (srcX+1, srcY-1)]
                WP -> [(srcX, srcY+1),(srcX-1, srcY+1),(srcX+1, srcY+1)]
                BK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
                WK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
