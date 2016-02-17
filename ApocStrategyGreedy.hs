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

{- | This is just a placeholder for the greedy strategy: it always chooses to play
     (0,0) to (2,1).
-}
--greedy  :: GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])
greedy    :: Chooser
greedy board Normal player = greedyNormal board player
greedy board PawnPlacement player = greedyPawnPlacement board player

greedyNormal :: board -> player -> IO (Maybe [(Int,Int)])
greedyNormal _ _ = return (Just[(0,0), (2,1)])

greedyPawnPlacement :: board -> player -> IO (Maybe [(Int,Int)])
greedyPawnPlacement _ _ = return (Just [(2,2)])

-- | Given the coordinates of a pawn, generates a list of tuples containing every possible move that pawn can legally make.
pawnValidMoves :: (Int, Int) -> board -> player -> [(Int),(Int)]
pawnValidMoves (srcX, srcY) board player = if (getFromBoard board (srcX, srcY + forwards)) == E)
                                          then
                      () = [(srcX, srcY +forwards)]
                | (getFromBoard board (srcX, srcY + forwards)) != Empty) = [()]
                where forwards = case of player of
                  Black -> -1
                  White -> 1

checkValidMoves :: Int -> [(Int, Int)] -> (Int, Int) -> Board -> Player -> [(Int, Int)]
checkValidMoves 0 xs (srcX, srcY) board player
                                                | getFromBoard board (srcX, srcY + forwards) == E = checkValidMoves counter+1 xs:[(srcX, srcY + forwards)]  board player
                                                | otherwise checkValidMoves counter+1 xs board player
                                                where forwards = case of player of
                                                  Black -> -1
                                                  White -> 1

checkValidMoves 1 xs (srcX, srcY) board player    | getFromBoard board (srcX+1 , srcY + forwards) && playerOf pieceTo /= player = checkValidMoves counter+1 xs:[(srcX, srcY + forwards)]  board player
                                                  | otherwise checkValidMoves counter+1 xs board player
                                                  where forwards = case of player of
                                                          Black -> -1
                                                          White -> 1
                                                        pieceTo = pieceOf $ getFromBoard board (srX, srcY)


checkValidMoves 2 xs (srcX, srcY) board player = | getFromBoard board (srcX, srcY + forwards) == E = checkValidMoves counter+1 xs:[(srcX, srcY + forwards)]  board player
| otherwise checkValidMoves counter+1 xs board player
where forwards = case of player of
  Black -> -1
  White -> 1

{-}-- | Determines the validity of a pawn movement
isPawnMoveValid :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isPawnMoveValid board player (fromX, fromY) to
       | to == (fromX, fromY + forwards) = True
       | to == (fromX + 1, fromY + forwards) && playerOf pieceTo /= player = True
       | to == (fromX - 1, fromY + forwards) && playerOf pieceTo /= player = True
       | otherwise = False
       where forwards = case player of
               Black -> -1
               White -> 1
             pieceTo = pieceOf $ getFromBoard board to-}


-- | Given the coordinates of a knight, generates a list of tuples containing every possible move that knight can legally make.
knightValidMoves :: (Int, Int) -> board -> [(Int), (Int)]


-- | Checks to see if an integer is within the board range
rangeChecker :: Int -> Bool
rangeChecker x = if ((x>4)||(x<0)) then False else True
