{- | This module is used for CPSC 449 for the Apocalypse assignment.

-}

module ApocStrategyGreedy where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import ApocHelpers
import ApocAIHelper
import System.Random


-- | This is the main function. It calls the greedyNormal function when a normal play is happening
-- or greedyPawnPlacement when a pawn palcement is happening.
greedy    :: Chooser
greedy gamestate Normal player = greedyNormal (theBoard gamestate) player
greedy gamestate PawnPlacement player = greedyPawnPlacement (theBoard gamestate) player

-- | Main function for pawn placement. Returns a single tuple
greedyPawnPlacement :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyPawnPlacement board player = return $ Just ([validPawnPlacement 0 0 board player])

-- | Main function for normal moves. Returns two tuples
greedyNormal :: Board -> Player -> IO (Maybe [(Int,Int)])
greedyNormal board player = do
  let moves = getAllPossibleMoves board player -- ^ gets all possible moves the player is currently able to make
  let sortedMoves = sortMoves board moves      -- ^ Sort them into SortedMoves data
  pickMove sortedMoves                        -- ^ Does the heavy lifting for the move

-- | Lets make the move!
pickMove :: SortedMoves -> IO (Maybe [(Int, Int)])
pickMove moves =  do
                  chance <- randomRIO (1, 100) :: IO Int -- ^ Generates a random Int from 1-100
                  (index, element) <- pickRandomMove moves -- ^ stores the move to make (in the list of SortedMoves)
                  if (chance > 90) -- ^ The chance is to prevent infinite loops.
                  then if (index < 0 || element < 0) -- ^ Ensures a move can be made
                      then return Nothing -- ^ if no move can be made 'passes'
                      else let move = ((getSortedMovesbyInt moves index) !! element) in return $ Just [fst move, snd move] -- ^ Gets move to be made, and returns it in the correct format.
                  else makeMove moves -- ^ Otherwise, make greedy move


-- | Greedy algoithm in play. Tries to capture highest point possible.
makeMove :: SortedMoves -> IO (Maybe [(Int, Int)])
makeMove moves
  -- | If there are knights to be taken, it will do it. pickRandom picks a random one of the knightMoves to play
  | length (knightMoves moves) > 0 = do
                                  move <- pickRandom (knightMoves moves)
                                  return $ Just [fst move, snd move]
  -- | Same concept as with the knights, checks pawn moves.
  | length (pawnMoves moves)   > 0 = do
                                  move <- pickRandom (pawnMoves moves)
                                  return $ Just [fst move, snd move]
  -- | If no piece can be taken, tries to make a random move to an empty position
  | length (emptyMoves moves)  > 0 = do
                                  move <- pickRandom (emptyMoves moves)
                                  return $ Just [fst move, snd move]
  -- | Otherwise passes.
  | otherwise = return $ Nothing

-- | Tries to pick a random move to play (in case where chance > 90)
pickRandomMove ::  SortedMoves -> IO (Int, Int)
pickRandomMove moves = do
                      -- | There probably is an easier way to do this function then this.
                      -- | Gets a random number for each of the elements, between their length and 0
                      element1 <- randomRIO (0, (length (getSortedMovesbyInt moves 0))-1)
                      element2 <- randomRIO (0, (length (getSortedMovesbyInt moves 1))-1)
                      element3 <- randomRIO (0, (length (getSortedMovesbyInt moves 2))-1)
                      -- | Tries to make a random move, started with knights and working its way down.
                      if (length (getSortedMovesbyInt moves 0) > 0)
                      then return (0, element1)
                      else  if (length (getSortedMovesbyInt moves 1) > 0)
                            then return (1, element2)
                            else  if (length (getSortedMovesbyInt moves 2) > 0)
                                  then return (2, element3)
                                  else return  (-1, -1) -- ^ If no move can be played, returns (-1, -1)
