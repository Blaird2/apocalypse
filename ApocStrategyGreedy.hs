{- | This module is used for CPSC 449 for the Apocalypse assignment.

-}

module ApocStrategyGreedy where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import ApocHelpers
import System.Random

type Move = ((Int, Int), (Int, Int))
data SortedMoves = SortedMoves {
                    emptyMoves  :: [Move],
                    pawnMoves   :: [Move],
                    knightMoves :: [Move]}


greedy    :: Chooser
greedy gamestate Normal player = greedyNormal (theBoard gamestate) player
greedy gamestate PawnPlacement player = greedyPawnPlacement (theBoard gamestate) player


greedyPawnPlacement :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyPawnPlacement board player = return $ Just ([validPawnPlacement 0 0 board player])

validPawnPlacement :: Int -> Int -> Board -> Player -> (Int, Int)
validPawnPlacement x 5 board player =  validPawnPlacement x 0 board player
validPawnPlacement x y board player = if (getFromBoard board (x,y) == E)
                                      then (x,y)
                                      else validPawnPlacement x (y+1) board player

greedyNormal :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyNormal board player = do
  let moves = getAllPossibleMoves board player
  let sortedMoves = sortMoves board moves
  pickMove sortedMoves

pickMove :: SortedMoves -> IO (Maybe [(Int, Int)])
pickMove moves =  do
                  chance <- randomRIO (1, 100) :: IO Int
                  (index, element) <- pickRandomMove moves
                  if (chance > 90)
                    then let move = ((getSortedMovesbyInt moves index) !! element) in return $ Just [fst move, snd move]
                      else makeMove moves



makeMove :: SortedMoves -> IO (Maybe [(Int, Int)])
makeMove moves
  | length (knightMoves moves) > 0 = do
                                  move <- pickRandom (knightMoves moves)
                                  return $ Just [fst move, snd move]
  | length (pawnMoves moves)   > 0 = do
                                  move <- pickRandom (pawnMoves moves)
                                  return $ Just [fst move, snd move]
  | length (emptyMoves moves)  > 0 = do
                                  move <- pickRandom (emptyMoves moves)
                                  return $ Just [fst move, snd move]
  | otherwise = return $ Nothing

pickRandomMove ::  SortedMoves -> IO (Int, Int)
pickRandomMove moves = do
                      element1 <- randomRIO (0, (length (getSortedMovesbyInt moves 0))-1)
                      element2 <- randomRIO (0, (length (getSortedMovesbyInt moves 1))-1)
                      element3 <- randomRIO (0, (length (getSortedMovesbyInt moves 2))-1)

                      if (length (getSortedMovesbyInt moves 0) > 0)
                      then return (0, element1)
                      else  if (length (getSortedMovesbyInt moves 1) > 0)
                            then return (1, element2)
                            else  if (length (getSortedMovesbyInt moves 2) > 0)
                                  then return (2, element3)
                                  else return  (-1, -1)

getSortedMovesbyInt :: SortedMoves -> Int -> [Move]
getSortedMovesbyInt moves index
    | index == 0 = emptyMoves moves
    | index == 1 = pawnMoves moves
    | index == 2 = knightMoves moves

pickRandom ::  [a] -> IO a
pickRandom list = do
  index <- randomRIO (0, (length list)-1)
  return (list !! index)


sortMoves :: Board -> [Move] -> SortedMoves
sortMoves _ [] = SortedMoves [] [] []
sortMoves board (x:xs) =
  let moves = sortMoves board xs
  in case (getFromBoard board (snd x)) of
    E ->  SortedMoves (x:(emptyMoves moves)) (pawnMoves moves) (knightMoves moves)
    BP -> SortedMoves (emptyMoves moves) (x:(pawnMoves moves)) (knightMoves moves)
    WP -> SortedMoves (emptyMoves moves) (x:(pawnMoves moves)) (knightMoves moves)
    BK -> SortedMoves (emptyMoves moves) (pawnMoves moves) (x:(knightMoves moves))
    WK -> SortedMoves (emptyMoves moves) (pawnMoves moves) (x:(knightMoves moves))

getAllPossibleMoves :: Board -> Player -> [Move]
getAllPossibleMoves board player =
  let pieces = getAllPieces board player
      moves = getMovesFromPieces board pieces
      validMoves = filter (\x -> isValidPlay board player (fst x) (snd x)) moves
  in validMoves


getAllPieces :: Board -> Player -> [(Int, Int)]
getAllPieces board Black = getInGrid (\x -> x == BK || x == BP) board 0
getAllPieces board White = getInGrid (\x -> x == WK || x == WP) board 0

getInGrid :: (a -> Bool) -> [[a]] -> Int -> [(Int, Int)]
getInGrid fn [] _ = []
getInGrid fn (x:xs) row = (map (\x -> (x, row)) $ getInRow fn x 0) ++ (getInGrid fn xs (row+1))

getInRow :: (a -> Bool) -> [a] -> Int -> [Int]
getInRow fn [] _ = []
getInRow fn (x:xs) col = if fn x then col:(getInRow fn xs (col+1)) else getInRow fn xs (col+1)

getMovesFromPieces :: Board ->  [(Int, Int)] -> [Move]
getMovesFromPieces _ [] = []
getMovesFromPieces board (x:xs) = let piece = filter ((rangeChecker).fst) (filter ((rangeChecker).snd) (getMovesFromSinglePiece board x)) in
                                  (createMoveFromSinglePiece x piece)++(getMovesFromPieces board xs)

rangeChecker :: Int -> Bool
rangeChecker x = if (x < 5 && x > -1) then True else False

createMoveFromSinglePiece :: (Int, Int) -> [(Int, Int)] -> [Move]
createMoveFromSinglePiece _ [] = []
createMoveFromSinglePiece src (x:xs) = (src, x):(createMoveFromSinglePiece src xs)

getMovesFromSinglePiece :: Board -> (Int, Int) -> [(Int, Int)]
getMovesFromSinglePiece board (srcX, srcY) = let piece = (srcX, srcY) in case (getFromBoard board (srcX, srcY)) of
                BP -> [(srcX, srcY-1),(srcX-1, srcY-1), (srcX+1, srcY-1)]
                WP -> [(srcX, srcY+1),(srcX-1, srcY+1),(srcX+1, srcY+1)]
                BK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
                WK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
