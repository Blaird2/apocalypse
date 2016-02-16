{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.Exit
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import ApocStrategyGreedy


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args
    | lenArgs == 0 = do
        strategies <- readStrategies
        putStrLn $ show initBoard
        gameLoop initBoard (fst strategies) (snd strategies)
    | lenArgs == 2 = do
        bStrat <- strategyFromName (head args)
        wStrat <- strategyFromName (last args)
        putStrLn $ show initBoard
        gameLoop initBoard bStrat wStrat
    | otherwise = die ("Invalid Strategies, Possible Strategies are:\n" ++ prtStrategyListFormat)
    where lenArgs = length args


---Game loop functions-------------------------------------------------------------

-- | Returns the printable list of available strategies
prtStrategyListFormat :: String
prtStrategyListFormat = (foldr (++) "" ((map (\x -> "  "++x++"\n") strategyList)))
    where strategyList = ["human", "greedy"]

-- | Converts a name to the respective strategy
strategyFromName :: String -> IO Chooser
strategyFromName "human" = return human
strategyFromName "greedy" = return greedy
strategyFromName _ = die ("Invalid Strategies, Possible Strategies are:\n" ++ prtStrategyListFormat)

-- | Queries for and reads the two strategies to be used from the command line
readStrategies :: IO (Chooser, Chooser)
readStrategies = do
    putStrLn "Possible Strategies:"
    putStr prtStrategyListFormat
    bStrat <- readStrategy "black"
    wStrat <- readStrategy "white"
    return (bStrat, wStrat)

-- | Queries for and reads a strategy from the command line
readStrategy :: String -> IO Chooser
readStrategy player = do
    putStrLn ("Enter the strategy for "++player++":")
    stratName <- getLine
    strategy <- strategyFromName stratName
    putStrLn stratName
    return strategy

-- | The game loop; plays out a full game using two given strategies, printing a trace as it runs, and ending once the game is over
gameLoop :: GameState -> Chooser -> Chooser -> IO ()
gameLoop state bStrat wStrat = do
    input <- getInput state bStrat wStrat
    let newState = nextGameState state input
    putStrLn $ show newState
    if isGameOver newState input then return () else gameLoop newState bStrat wStrat

-- | Reads the next moves from the strategies, and converts them into Played values, checking if they were valid
getInput :: GameState -> Chooser -> Chooser -> IO (Played, Played)
getInput state bStrat wStrat = do
    -- TODO - pawn placement?
    bMove <- bStrat state Normal Black
    wMove <- wStrat state Normal White
    -- TODO - check if valid, convert to Played
    return (Passed, Passed)

-- | Generates the next game state, using the moves chosen by the two strategies
nextGameState :: GameState -> (Played, Played) -> GameState
nextGameState state (bMove, wMove) = GameState
    bMove
    ((blackPen state) + (getPenaltyValue bMove))
    wMove
    ((whitePen state) + (getPenaltyValue wMove))
    (theBoard state) -- TODO - apply the moves

-- | Determines if the game is now over; either because a player has accumulated a penalty
isGameOver :: GameState -> (Played, Played) -> Bool
isGameOver state (bMove, wMove)
         | bMove == Passed && wMove == Passed = True
         | blackPen state >= 2 = True
         | whitePen state >= 2 = True
         | count2 (theBoard state) BP == 0 = True
         | count2 (theBoard state) WP == 0 = True
         | otherwise = False

-- | Determines which player has won; 'Nothing' means a draw if isGameOver is true, otherwise it means the game isn't over yet
getWinner :: GameState -> (Played, Played) -> Maybe Player
getWinner state (bMove, wMove)
        | bMove == Passed && wMove == Passed =
            if bPawns > wPawns
                then Just Black
                else if wPawns > bPawns
                    then Just White
                    else Nothing
        | whiteLoss && not blackLoss = Just Black
        | blackLoss && not whiteLoss = Just White
        | otherwise = Nothing
        where bPawns = count2 (theBoard state) BP
              wPawns = count2 (theBoard state) WP
              blackLoss = blackPen state >= 2 || count2 (theBoard state) BP == 0
              whiteLoss = whitePen state >= 2 || count2 (theBoard state) WP == 0

-- | Returns the penalty value of a given move
getPenaltyValue :: Played -> Int
getPenaltyValue (Played _)              = 0
getPenaltyValue (Passed)                = 0
getPenaltyValue (Goofed _)              = 1
getPenaltyValue (Init)                  = 0
getPenaltyValue (UpgradedPawn2Knight _) = 0
getPenaltyValue (PlacedPawn _)          = 0
getPenaltyValue (BadPlacedPawn _)       = 1
getPenaltyValue (NullPlacedPawn)        = 0
getPenaltyValue (None)                  = 0


---Movement Checking functions------------------------------------------------------

-- | Determines whether the move was a normal turn, or a pawn placement
isValidMove :: Board -> Player -> [(Int, Int)] -> Bool
isValidMove board player [x, y] = isValidPlay board player x y
isValidMove board _ [x] = isValidPlacePawn board x

-- | Determines all the valid moves for the pieces on the board
isValidPlay :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isValidPlay board player from to
    -- TODO - maybe range check 'to'?
    | playerOf pieceFrom == player && (pieceFrom == WhitePawn || pieceFrom == BlackPawn) =
        isPawnMoveValid board player from to
    | playerOf pieceFrom == player && (pieceFrom == WhiteKnight || pieceFrom == BlackKnight) =
        isKnightMoveValid from to
    | otherwise = False
    where pieceFrom = pieceOf $ getFromBoard board from
          pieceTo = pieceOf $ getFromBoard board to

-- | Returns whether the pawn placement was a valid move
isValidPlacePawn :: Board -> (Int, Int) -> Bool
isValidPlacePawn board x = getFromBoard board x == E

-- | Determines the validity of a pawn movement
isPawnMoveValid :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
isPawnMoveValid board player (fromX, fromY) to
       | to == (fromX, fromY + forwards) = True
       | to == (fromX + 1, fromY + forwards) && playerOf pieceTo /= player = True
       | to == (fromX - 1, fromY + forwards) && playerOf pieceTo /= player = True
       | otherwise = False
       where forwards = case player of
               Black -> -1
               White -> 1
             pieceTo = pieceOf $ getFromBoard board to

-- | Determines the validity of a knight movement
isKnightMoveValid :: (Int, Int) -> (Int, Int) -> Bool
isKnightMoveValid (fromX, fromY) to
         | to == (fromX + 1, fromY + 2) = True
         | to == (fromX + 1, fromY - 2) = True
         | to == (fromX - 1, fromY + 2) = True
         | to == (fromX - 1, fromY - 2) = True
         | to == (fromX + 2, fromY + 1) = True
         | to == (fromX + 2, fromY - 1) = True
         | to == (fromX - 2, fromY + 1) = True
         | to == (fromX - 2, fromY - 1) = True
         | otherwise = False


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
    in (if null zs then (if null ys then [] else init ys) else ys)
        ++ [elem]
        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2 :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- | Counts the occurences of an element in a list
count :: (Eq a) => [a] -> a -> Int
count xs x = length $ filter (== x) xs

-- | Counts the occurences of an element in a list of lists
count2 :: (Eq a) => [[a]] -> a -> Int
count2 xs x = sum $ map (\xs -> count xs x) xs
