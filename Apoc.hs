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
        gameLoop initBoard (fst strategies) (snd strategies)
    | lenArgs == 2 = do
        bStrat <- strategyFromName (head args)
        wStrat <- strategyFromName (last args)
        gameLoop initBoard bStrat wStrat
    | otherwise = die ("Invalid Strategies, Possible Strategies are:\n" ++ prtStrategyListFormat)
    where lenArgs = length args

---Movement Checking functions------------------------------------------------------

-- | Determines whether the move was a normal turn, or a pawn placement
isValidMove :: GameState -> Player -> [a] -> Bool
isValidMove b _ x:[] = isValidPlacePawn (theboard b) x
isValidMove b player x:xs:[] = isValidPlay (theboard b) x xs

-- | Returns whether the pawn placement was a valid move
isValidPlacePawn :: Board -> (Int, Int) -> Bool
isValidPlacePawn board x = if((getFromBoard board x) == E ) then True else False

-- | Determines all the valid moves for the pieces on the board
isValidPlay :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool 
isValidPlay board player from to
    | (((playerOf piece) == player) && (piece == WP)) = ((fst to == fst from) && (snd to == ((snd from)+1))) || (((playerOf (pieceOf (getFromBoard to))) == Black) && (((fst from)+1) || ((fst from)-1)) && (snd to == ((snd from)+1)))
    | (((playerOf piece) == player) && (piece == BP)) = ((fst to == fst from) && (snd to == ((snd from)-1))) || (((playerOf (pieceOf (getFromBoard to))) == White) && (((fst from)+1) || ((fst from)-1)) && (snd to == ((snd from)-1)))
    | (((playerOf piece) == player) && (piece == WK)) = knightMove from to
    | (((playerOf piece) == player) && (piece == BK)) = knightMove from to
    | otherwise = False
    where piece = getFromBoard board from 

-- | Determines the validity of a knight movement
knightMove :: (Int, Int) -> (Int, Int) -> Bool
knightMove from to 
    | ((fst to == ((fst from)+2)) && ((snd to == ((snd from)-1)) || snd to == ((snd from)+1))) = True
    | ((fst to == ((fst from)-2)) && ((snd to == ((snd from)-1)) || snd to == ((snd from)+1))) = True
    | ((snd to == ((snd from)+2)) && ((fst to == ((fst from)-1)) || fst to == ((fst from)+1))) = True
    | ((snd to == ((snd from)-2)) && ((fst to == ((fst from)-1)) || fst to == ((fst from)+1))) = True
    | otherwise = False

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
    newState <- nextGameState state bStrat wStrat
    putStrLn $ show newState
    if isGameOver newState then return () else gameLoop newState bStrat wStrat

-- | Generates the next game state, using the moves chosen by the two strategies
nextGameState :: GameState -> Chooser -> Chooser -> IO GameState
nextGameState state bStrat wStrat = do
    -- TODO - get and apply the moves
    return $ GameState
        (Passed)
        ((blackPen state)+1)
        (Passed)
        (whitePen state)
        (theBoard state)

-- | Determines if the game is now over; either because a player has accumulated a penalty
isGameOver :: GameState -> Bool
isGameOver state = (blackPen state) >= 2 || (whitePen state) >= 2 || count2 (theBoard state) BP == 0 || count2 (theBoard state) WP == 0

-- | Determines which player has won; 'Nothing' means a draw if isGameOver is true, otherwise it means the game isn't over yet
getWinner :: GameState -> Maybe Player
getWinner state
        | whiteLoss && not blackLoss = Just Black
        | blackLoss && not whiteLoss = Just White
        | otherwise = Nothing
        where blackLoss = blackPen state >= 2 || count2 (theBoard state) BP == 0
              whiteLoss = whitePen state >= 2 || count2 (theBoard state) WP == 0


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
