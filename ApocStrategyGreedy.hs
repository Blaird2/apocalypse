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
greedyPawnPlacement _ _ = return (Just [(2,2)])

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
      moves = getMovesFromPieces pieces
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

getMovesFromPieces :: [(Int, Int)] -> [Move]
getMovesFromPieces pieces = []

{-
getAllPossibleMoves :: Board -> Player -> [((Int, Int), [(Int, Int)])]
getAllPossibleMoves board player = zip (isWhite board) (map (\x -> moveChecker x board player) (isWhite board))

unflattenPossibleMoves :: [(Int, Int)] ->  [[(Int, Int)]] -> [Move]
unflattenPossibleMoves [] _ = []
unflattenPossibleMoves (x:xs) [(y:ys)]= (x,y):unflattenPossibleMoves xs (if (null y) then ys else ys)

isBlack :: Board -> [(Int, Int)]
isBlack board = isHelper 0 0 Black board

isWhite :: Board -> [(Int, Int)]
isWhite board = isHelper 0 0 White board

isHelper :: Int -> Int -> Player -> Board -> [(Int, Int)]
isHelper 5 _ _ _ = []
isHelper x 5 player board = isHelper (x+1) 0 player board
isHelper x y player board = if (player == Black)
                            then  if (getFromBoard board (x, y)==BP || getFromBoard board (x, y)==BK)
                                  then (x,y):isHelper x (y+1) player board
                                  else isHelper x (y+1) player board
                            else  if (getFromBoard board (x, y)==WP || getFromBoard board (x, y)==WK)
                                  then (x,y):isHelper x (y+1) player board
                                  else isHelper x (y+1) player board

moveChecker :: (Int, Int) -> Board -> Player -> [(Int, Int)]
moveChecker (srcX, srcY) board player = if (getFromBoard board (srcX, srcY) == BP || getFromBoard board (srcX, srcY) == WP)
                                        then pawnMoveChecker pawnPossibleMoves pawnExpectedPiece (srcX, srcY) board player
                                        else knightMoveChecker knightPossibleMoves (srcX, srcY) board player

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


pawnEmptytoPiece :: String -> Player -> (Cell, Cell)
pawnEmptytoPiece "E" _ = (E, E)
pawnEmptytoPiece "NE" White = (BK, BP)
pawnEmptytoPiece "NE" Black = (WK, WP)

-- | These two statements allow for ease of checking valid moves.
pawnPossibleMoves = [(0,1), (1,1), (1,-1)]
pawnExpectedPiece  = ["E", "NE", "NE"]
knightPossibleMoves = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]

-- | Checks to see if an integer is within the board range
rangeChecker :: Int -> Bool
rangeChecker x = if ((x>4)||(x<0)) then False else True
-}
