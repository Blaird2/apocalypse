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

module ApocStrategyHuman where

import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human state play player = if (play == PawnPlacement) then doPawnPlacementMove state player else (doNormalMove state player)

-- | Pawn placement move, returns where the user desires to place pawn.
-- Checks for misformed input, but does not check legality of move.
-- Must pass in type Chooser without play type.
doPawnPlacementMove :: GameState -> Player -> IO (Maybe [(Int,Int)])
doPawnPlacementMove state player = do
  let playerStr = if (player == White) then "W" else "B"
  putStrLn ("Enter the coordinates to place the pawn for player " ++ (show player) ++" in the form 'destX destY':\n [0 >= n >= 4]" ++ playerStr ++"2: ")
  move <- getLine
  let moveInt = convertMovetoIntList 2 move
  if (length (moveInt) == length (filter rangeChecker (moveInt)))
    then do
      putStrLn move
      return (Just (listReturn moveInt))
      else if (length (filter rangeChecker (moveInt)) < 2)
        then do
          putStrLn (move ++" integers out of range")
          doPawnPlacementMove state player
        else do
          putStrLn (move ++ " "++ (convertLengthToString moveInt) ++" integers found, 2 required.")
          doPawnPlacementMove state player

-- | Regular move, returns where the user desires to place pawn/knight.
-- Checks for misformed input, but does not check legality of move.
-- Must pass in type Chooser without play type.
doNormalMove ::  GameState -> Player -> IO (Maybe [(Int,Int)])
doNormalMove state player = do
  let playerStr = if (player == White) then "W" else "B"
  putStrLn ("Enter the move coordinates for player "++ (show player) ++" in the fo rm 'srcX srcY destX destY'\n (0 >= n >= 4, or just enter return for a 'pass') " ++ playerStr ++"2: ")
  move <- getLine
  let moveInt = convertMovetoIntList 4 move
  if (length (moveInt) == length (filter rangeChecker (moveInt)))
    then do
        putStrLn ("Valid coordinates")
        return (Just (listReturn moveInt))
    else if (length (filter rangeChecker (moveInt)) < 4)
        then do
            putStrLn (move ++" integers out of range")
            doNormalMove state player
        else do
          putStrLn (move ++ " "++ (convertLengthToString moveInt) ++" integers found, 4 required.")
          doNormalMove state player


-- | Helper functions

-- | Converts the length of a list into the corresponding word.
convertLengthToString :: [Int] -> String
convertLengthToString xs
            | len == 1 = "One"
            | len == 2 = "Two"
            | len == 3 = "Three"
            | len == 5 = "Five"
            -- | Bounding cases.
            | len >  5 = "Greater then five"
            | len <  1 = "Less then one"
            where len = length xs

-- | Takes a list of coordinates and returns them in tuple format
listReturn :: [Int] -- ^ @[srcX, srcY, 'dstX, dstY]@
              -> [(Int, Int)] -- ^ [(srcX, srcY), (dstX, dstY)]
listReturn [a,b] = [(a, b)] -- ^ Base case. 2 elements into one tuple
listReturn (a:b:xs) = (a,b):listReturn xs -- ^ Recursive case. 4 elements into one tuple, recursiving into 2.

-- | Converts a string into a list of ints for the move. Ignores string after int provided
convertMovetoIntList :: Int -- ^ How many arguments are we expecting (2 or 4)
                        -> String -- ^ The Move that the user inputted
                        -> [Int] -- ^ Resulting list. Ex. [2,2,3,4]
-- | First we convert string into list of strings, then ignore what is after the int passed in.
-- Then we convert each string inside this list, into an integer - expecting a list of ints.
convertMovetoIntList amount xs = map read (take amount (words xs)) :: [Int]

-- | Checks to see if an integer is within the board range
rangeChecker :: Int -> Bool
rangeChecker x = if ((x>4)||(x<0)) then False else True
