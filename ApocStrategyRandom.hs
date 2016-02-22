


module ApocStrategyRandom where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools
import ApocHelpers
import ApocAIHelper
import System.Random


random :: Chooser
random gamestate Normal player = randomNormal (theBoard gamestate) player
random gamestate PawnPlacement player = randomPawnPlacement (theBoard gamestate) player

-- | Main function for pawn placement. Returns a single tuple
randomPawnPlacement :: Board -> Player -> IO (Maybe [(Int,Int)])
randomPawnPlacement board player = return $ Just ([validPawnPlacement 0 0 board player])

randomNormal :: Board -> Player -> IO (Maybe [(Int,Int)])
randomNormal board player = do
                      let moves = getAllPossibleMoves board player -- ^ gets all possible moves the player is currently able to make
                      let sortedMoves = sortMoves board moves      -- ^ Sort them into SortedMoves data
                      pickMove sortedMoves                        -- ^ Does the heavy lifting for the move

-- | Tries to pick a random move to play
pickRandomMove ::  SortedMoves -> IO (Int, Int)
pickRandomMove moves = do
                      -- | There probably is an easier way to do this function then this.
                      -- | Gets a random number for each of the elements, between their length and 0
                      indextoStart <- randomRIO (0, 2) -- ^ Random start not nessarily on knights
                      element1 <- randomRIO (0, (length (getSortedMovesbyInt moves 0))-1)
                      element2 <- randomRIO (0, (length (getSortedMovesbyInt moves 1))-1)
                      element3 <- randomRIO (0, (length (getSortedMovesbyInt moves 2))-1)
                      -- | Tries to make a random move, started with knights and working its way down.
                      if (length (getSortedMovesbyInt moves ((indextoStart) `mod` 2)) > 0)
                      then return (0, element1)
                      else  if (length (getSortedMovesbyInt moves ((indextoStart+1) `mod` 2)) > 0)
                            then return (1, element2)
                            else  if (length (getSortedMovesbyInt moves ((indextoStart+2) `mod` 2)) > 0)
                                  then return (2, element3)
                                  else return  (-1, -1) -- ^ If no move can be played, returns (-1, -1)

-- | Lets make the move!
pickMove :: SortedMoves -> IO (Maybe [(Int, Int)])
pickMove moves =  do
                    chance <- randomRIO (1, 100) :: IO Int -- ^ Generates a random Int from 1-100
                    (index, element) <- pickRandomMove moves -- ^ stores the move to make (in the list of SortedMoves)
                    if (index < 0 || element < 0) -- ^ Ensures a move can be made
                    then return Nothing -- ^ if no move can be made 'passes'
                    else let move = ((getSortedMovesbyInt moves index) !! element) in return $ Just [fst move, snd move] -- ^ Gets move to be made, and returns it in the correct format.
