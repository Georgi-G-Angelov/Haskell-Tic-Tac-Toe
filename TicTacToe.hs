module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-- The game is fully playable. Also, more testing has been done.
-- There are some new functions introduced in order to make the game work.
-- For example, emptyBoard generates an empty board of a certain size.
-- Testing has been done for the new functions as well.

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

-- Checks whether there is a winner.
gameOver :: Board -> Bool
gameOver ([], _) = True
gameOver board
  | winningState (diags board) = True
  | winningState (cols board)  = True
  | winningState (rows board)  = True
  | otherwise                  = False
    where
      winningState :: [[Cell]] -> Bool
      winningState [] = False
      winningState (l:ls)
        | head l /= Empty && all (==head l) l = True
        | otherwise                           = winningState ls

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
--parsePosition :: String -> Maybe Position
parsePosition :: String -> Maybe Position
parsePosition input
  = parsePosition' first second
      where
        parsePosition' :: String -> String -> Maybe Position
        parsePosition' word1 word2
          = do
              i <- readMaybe word1
              j <- readMaybe word2
              return (i, j)

        first  = takeWhile (/=' ') input
        second = drop 1 (dropWhile (/=' ') input)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (i,j) board@(cells, size)
  | i<size && i>=0 && j>=0 && j<size && cells !! (i*size + j) == Empty
    = Just (replace (i*size + j) (Taken player) cells, size)
  | otherwise
    = Nothing
-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint board@(cells, size)
  = prettyPrint' (rows board)
        where
          prettyPrint' :: [[Cell]] -> IO()
          prettyPrint' [] = putStr ""
          prettyPrint' (row : rows)
            = do
                putStrLn (intersperse ' ' (toString row))
                prettyPrint' rows

          toString :: [Cell] -> String
          toString [] = ""
          toString (cell:cells)
            | cell == Empty   = '-' : toString cells
            | cell == Taken O = 'O' : toString cells
            | cell == Taken X = 'X' : toString cells

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board player
  = do
      position <- getLine
      let move = tryMove player (fromJust (parsePosition position)) board
      if parsePosition position == Nothing
        then do
          putStr "Please enter a valid position:"
          takeTurn board player
        else do
      if move == Nothing
        then do
          putStr "Invalid move. Please enter a valid move:"
          takeTurn board player
        else do
          return (fromJust move)


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.

playGame :: Board -> Player -> IO ()
playGame board player
  = do
      putStrLn ("It is " ++ show player ++ "'s turn!")
      putStr "Enter a board position:"
      newBoard <- takeTurn board player
      prettyPrint newBoard
      if (gameOver newBoard)
        then
          putStrLn ("Congratulations! The winner is " ++ show player ++ "!")
        else do
      if isFull newBoard
        then
          putStrLn ("It is a draw!")
        else do
          playGame newBoard otherPlayer
          return ()

        where
          otherPlayer
            | player == X = O
            | player == O = X


-- Checks whether all of the cells of a board are taken.
isFull :: Board -> Bool
isFull (cells, size)
  = all (/=Empty) cells


-- Reads a valid board size from the keyboard.
getSize :: IO Int
getSize
  = do
      size <- getLine
      let maybeSize = readMaybe size :: Maybe Int
      if maybeSize == Nothing || (fromJust maybeSize) <= 0
        then do
          putStr "Invalid size. Please enter a new size:"
          getSize
        else
          return (fromJust maybeSize)

-- Generates an empty board of a certain size.
emptyBoard :: Int -> Board
emptyBoard size
  | size > 0  = (emptyBoard' (size*size), size)
  | otherwise = error "Invalid size."
    where
      emptyBoard' :: Int -> [Cell]
      emptyBoard' 0 = []
      emptyBoard' n = Empty : emptyBoard' (n-1)

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStr "Welcome! Enter the dimensions of the board:"
      size <- getSize
      let board = emptyBoard size
      prettyPrint board
      playGame board X
      return ()

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3, testBoard4, testBoard5, testBoard6 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard4
  = ([Taken O, Taken X, Taken O,
      Empty  , Taken O, Taken X,
      Taken O, Empty  , Taken X],
      3)

testBoard5
  = ([Taken O, Empty  , Taken O,
      Empty  , Taken O, Taken X,
      Taken X, Empty  , Taken X],
      3)

testBoard6
  = ([Taken X, Taken X,
      Taken X, Taken X],
      2)
