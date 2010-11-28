-- Rutger Storm
-- F093490

module Main where

import System.Random  -- for randoms
import System.IO      -- for hFlush
import Data.List (sort)

type Row = [Int]
type Guess = Row
type Solution = Row

colors = 6
width  = 4

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    s <- generateSolution -- initialization
    loop s 1               -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: (Num a)=>Solution -> a -> IO ()
loop s tries =
  do
    i <- input
    if (i == [0])
       then do putStrLn $ "Terminiating Game, Thank you for playing...\n\nYou tried: " ++show tries ++ " times.\n\nThe correct solution was: " ++ (show s)
       else do
              let result = check s i
              putStrLn (report result)
              if (win result)
                 then do putStrLn ("It took you " ++ show tries ++ " tries\n")
                 else loop s (tries+1)
            where win(_,_,b) = b

-- You could also have written the black function with zipWith but
-- the current black function was more readable for me.
black, white :: Solution -> Guess -> Int
black solution guess = length (blackMatch solution guess)

blackMatch :: Solution -> Guess -> [Int]
blackMatch [] _ = []
blackMatch _ [] = []
blackMatch (x:xs) (y:ys)
             |(x == y) = x : (blackMatch xs ys)
             |otherwise = blackMatch xs ys

-- This function calculates white by first checking how many
-- elements are the same and by the subtracting black.			 
white solution guess = length (whiteMatch solution guess) - (black solution guess)
whiteMatch x y = (whiteMatch' (sort x) (sort y))
whiteMatch' [] _ = []
whiteMatch' _ [] = []
whiteMatch' (x:xs) (y:ys)
  | (x == y) = x : (whiteMatch' xs ys)
  | (x < y) = whiteMatch' xs (y:ys)
  | (x > y) = whiteMatch' ys (x:xs)


check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?

check solution guess = check' (black solution guess)
   where check' blackNum = (blackNum, white solution guess, blackNum == 4)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (black,white,True) = show black ++ " black, " ++ show white ++ " white\nCongratulations"
report (black,white,_) = show black ++ " black, " ++ show white ++ " white"

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
-- 
-- Added getGuess to make reading the code easier.
-- guess == [0] stands for giving up.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    guess <- getGuess
    if (guess == [0]) 
        then (return guess)
        else if (valid guess) then (return guess)
                else do putStrLn "Invalid input"
                        input
    
-- This function is a separation from the input function.
-- The use of this function is that it changes a input string to a list
-- of ints.
getGuess :: IO Guess
getGuess = do line <- getLine
              if line == "q" || line == "quit" || line == "e" || line == "end"
                 then return ([0])
                 else return (map (readInt) (words line))
              
-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = (all c guess) && ((length guess) == width)
                where c a = (a>0) && (a<=colors)