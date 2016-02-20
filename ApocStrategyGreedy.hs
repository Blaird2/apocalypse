{- | This module is used for CPSC 449 for the Apocalypse assignment.

This is merely a skeleton to get you started.  It has VERY little functionality.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module ApocStrategyGreedy where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import ApocHelpers

--import System.Random

type Move = ((Int, Int), (Int, Int))
data SortedMoves = SortedMoves {emptyMoves  :: [Move],
                    pawnMoves   :: [Move],
                    knightMoves :: [Move]}

--greedy  :: GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])
greedy    :: Chooser
greedy gamestate Normal player = greedyNormal (theBoard gamestate) player
greedy gamestate PawnPlacement player = greedyPawnPlacement (theBoard gamestate) player

greedyNormal :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyNormal board player = do
  let moves = getAllPossibleMoves board player
  let sortedMoves = sortMoves board moves
  pickMove sortedMoves

greedyPawnPlacement :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyPawnPlacement board player = do
  let pieces = getAllPieces board player
  let validMoves = filter (\x -> validPawnPlacement board player x) pieces
  if (length validMoves > 0 ) then return $ Just ([snd $ head $ getPossibleMove board player validMoves]) else return Nothing



validPawnPlacement :: Board -> Player -> (Int, Int) -> Bool
validPawnPlacement board player (x,y) = if (player == Black)
                                        then  if (x == 0 && ((checkKnightCount board player) < 2) && (getFromBoard board (x,y) == BP))
                                              then True
                                              else False
                                        else  if (x == 4 && ((checkKnightCount board player) < 2) && (getFromBoard board (x,y) == WP))
                                              then True
                                              else False

checkKnightCount :: Board -> Player -> Int
checkKnightCount board player = count2 board (if (player == Black) then BK else WK)


pickMove :: SortedMoves -> IO (Maybe [(Int, Int)])
pickMove moves
  | length (knightMoves moves) > 0 = let move = head (knightMoves moves) in return $ Just [fst move, snd move]
  | length (pawnMoves moves)   > 0 = let move = head (pawnMoves moves)   in return $ Just [fst move, snd move]
  | length (emptyMoves moves)  > 0 = let move = head (emptyMoves moves)  in return $ Just [fst move, snd move]
  | otherwise = return $ Nothing

{-}
pickRandom ::  [a] -> IO a
pickRandom list = do
  index <- getStdRandom (randomR, (0, (length list) -1))
  return (list !! index)
-}

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

getPossibleMove :: Board -> Player -> [(Int, Int)] -> [Move]
getPossibleMove board player piece=
      let moves = getMovesFromPieces board piece
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

-- | TODO: Double check if possible moves are legal moves possible.
getMovesFromSinglePiece :: Board -> (Int, Int) -> [(Int, Int)]
getMovesFromSinglePiece board (srcX, srcY) = let piece = (srcX, srcY) in case (getFromBoard board (srcX, srcY)) of
                BP -> [(srcX, srcY-1),(srcX-1, srcY-1), (srcX+1, srcY-1)]
                WP -> [(srcX, srcY+1),(srcX-1, srcY+1),(srcX+1, srcY+1)]
                BK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
                WK -> [(srcX+1,srcY+2), (srcX+1, srcY-2), (srcX-1, srcY+2), (srcX-1, srcY-2), (srcX+2, srcY+1), (srcX+2, srcY-1), (srcX-2, srcY+1), (srcX-2, srcY-1)]
