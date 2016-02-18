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



--greedy  :: GameState -> PlayType -> Player -> IO (Maybe [(Int,Int)])
greedy    :: Chooser
greedy gamestate Normal player = greedyNormal (theBoard gamestate) player
greedy gamestate PawnPlacement player = greedyPawnPlacement (theBoard gamestate) player

greedyNormal :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyNormal _ _ = return (Just[(0,0), (2,1)])

greedyPawnPlacement :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyPawnPlacement _ _ = return (Just [(2,2)])

-- | These two statements allow for ease of checking valid moves.
pawnPossibleMoves = [(0,1), (1,1), (1,-1)]
pawnExpectedPiece  = ["E", "NE", "NE"]
knightPossibleMoves = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]



pawnMoveChecker :: [(Int, Int)] -> [String] -> (Int, Int) -> Board -> Player -> [(Int, Int)]
pawnMoveChecker [] _ _ _ _ = []
pawnMoveChecker ((posX, posY):xs) (e:es) (srcX, srcY) board player = if (player == Black)
                                                              then  if    (getFromBoard board (srcX+posX, srcY+posY)==(fst (pawnEmptytoPiece e player)) ||  getFromBoard board (srcX+posX, srcY+posY)==(snd (pawnEmptytoPiece e player)))
                                                                then  (srcX+posX, srcY+posY):(pawnMoveChecker xs es (srcX, srcY) board player)
                                                                else  pawnMoveChecker xs es (srcX, srcY) board player
                                                              else  if    (getFromBoard board (srcX-posX, srcY-posY)==(fst (pawnEmptytoPiece e player)) ||  getFromBoard board (srcX-posX, srcY-posY)==(snd (pawnEmptytoPiece e player)))
                                                                then  (srcX-posX, srcY-posY):(pawnMoveChecker xs es (srcX, srcY) board player)
                                                                else  pawnMoveChecker xs es (srcX, srcY) board player


knightMoveChecker :: [(Int, Int)] -> (Int, Int) -> Board -> Player -> [(Int, Int)]
knightMoveChecker [] _ _ _ = []
knightMoveChecker ((posX, posY):xs) (srcX, srcY) board player = if (player == Black)
                                                                then  if (getFromBoard board (srcX+posX, srcY+posY)/=(BK) || getFromBoard board (srcX+posX, srcY+posY)/=(BP))
                                                                      then (srcX+posX, srcY+posY):(knightMoveChecker xs (srcX, srcY) board player)
                                                                      else knightMoveChecker xs (srcX, srcY) board player
                                                                else  if (getFromBoard board (srcX-posX, srcY-posY)/=(WK) || getFromBoard board (srcX-posX, srcY-posY)/=(WP))
                                                                      then (srcX-posX, srcY-posY):(knightMoveChecker xs (srcX, srcY) board player)
                                                                      else knightMoveChecker xs (srcX, srcY) board player


{-
-- | validMoveChecker:: Every possible move the pawn/knight can make -> List of expected returns values -> Current Location -> the Board -> Player -> Returns list of possible moves this pawn can make
validMoveChecker :: [(Int, Int)] -> [String]-> (Int, Int)-> Board -> Player -> [(Int, Int)]
validMoveChecker [] _ _ _ _ = []
validMoveChecker ((posX, posY):xs) (curCell:cells) (srcX, srcY) board player =  if (player == Black)
                                                                then  if    (getFromBoard board (srcX+posX, srcY+posY)==)
                                                                      then  (srcX+posX, srcY+posY):(validMoveChecker xs cells (srcX, srcY) board player)
                                                                      else  validMoveChecker xs cells (srcX, srcY) board player
                                                                else  if    (getFromBoard board (srcX-posX, srcY-posY)==curCell)
                                                                      then  (srcX-posX, srcY-posY):(validMoveChecker xs cells (srcX, srcY) board player)
                                                                      else  validMoveChecker xs cells (srcX, srcY) board player
-}
pawnEmptytoPiece :: String -> Player -> (Cell, Cell)
pawnEmptytoPiece "E" _ = (E, E)
pawnEmptytoPiece "NE" White = (BK, BP)
pawnEmptytoPiece "NE" Black = (WK, WP)


-- | Checks to see if an integer is within the board range
rangeChecker :: Int -> Bool
rangeChecker x = if ((x>4)||(x<0)) then False else True
