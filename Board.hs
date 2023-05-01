--Alexander Watson and Nathanael Perez
--CS 3360
--Dr. Cheon
--Board Module

module Board where

  --Create an board of n rows and n colums
  mkBoard :: Int -> [[String]]
  mkBoard n = replicate n (replicate n ".")

  --Create and return the first player
  mkPlayer :: String
  mkPlayer = "1"

  --Create and return the second player
  mkOpponent :: String
  mkOpponent = "2"

  --Return the size of the board
  size :: [a] -> Int
  size = length

  -- Return row
  row :: Int -> [[a]] -> [a]
  row y bd = bd !! (y - 1)

  -- returns the column
  column :: Int -> [[a]] -> [a]
  column x bd = [ row y bd !! (x - 1) | y <- [1..size bd] ]

  --Returns left diagonal array
  --Moves through the diagonal row by row
  nextRowLeftDiag :: Int -> [[a]] -> Int -> [a]
  nextRowLeftDiag x bd n = if n > length bd || x == 0 then [] else (reverse (row n bd) !! (x - 1)) : nextRowLeftDiag (x - 1) bd (n + 1)
  
  ldiag :: Int -> [[a]] -> [a]
  ldiag x bd
    | x == 0 = []
    | x <= length bd = (reverse (row 1 bd) !! (x - 1)) : nextRowLeftDiag (x - 1) bd 2
    | otherwise = (reverse (row (x - length bd + 1) bd) !! (length bd - 1)) : nextRowLeftDiag (length bd - 1) bd (x - length bd + 2)

  --Returns anti diagonal array
  --Moves through the diagonal row by row
  nextRowRightDiag :: Int -> [[a]] -> Int -> [a]
  nextRowRightDiag x bd n = if n > length bd || x == 0 then [] else (row n bd !! (x - 1)) : nextRowRightDiag (x - 1) bd (n + 1)

  rdiag :: Int -> [[a]] -> [a]
  rdiag x bd
    | x == 0 = []
    | x <= length bd = (row 1 bd !! (x - 1)) : nextRowRightDiag (x - 1) bd 2
    | otherwise = (row (x - length bd + 1) bd !! (length bd - 1)) : nextRowRightDiag (length bd - 1) bd (x - length bd + 2)

  --Mark a place (x,y) in a board bd by a player p
  mark :: Int -> Int -> [[a]] -> a -> [[a]]
  mark x y bd p = take (y - 1) bd ++ [take (x - 1) (row y bd) ++ [p] ++ drop x (row y bd)] ++ drop y bd

  -- isEmpty x y bd - Is a place (x,y) of a board bd unmarked or a stone not placed?
  isEmpty :: Int -> Int -> [[String]] -> Bool
  isEmpty x y bd = row y bd !! (x - 1) == "."

  --isMarked x y bd - Does a place (x,y) of a board bd have a stone placed
  isMarked :: Int -> Int -> [[String]] -> Bool
  isMarked x y bd = row y bd !! (x - 1) /= "."


  --isMarkedBy x y bd p - Does a place (x,y) of a board bd have a stone placed by a player p
  isMarkedBy :: Eq a => Int -> Int -> [[a]] -> a -> Bool
  isMarkedBy x y bd p = row y bd !! (x - 1) == p

  --Return the player of the stone placed on a place (x,y) of a board bd
  marker :: Int -> Int -> [[a]] -> a
  marker x y board = row y board !! (x - 1)

  --Check if board is full
  isFull :: [[String]] -> Bool
  isFull = all (notElem ".")

  --Check if won conditions
  isWonBy :: Eq t => [[t]] -> t -> Bool
  isWonBy bd p = rowsAndCols bd 1 0 0 p || leftAndRightDiag bd 1 0 0 p

  --Checks rows and columns per player
  rowsAndCols :: (Eq t1, Num t2, Num t3) => [[t1]] -> Int -> t2 -> t3 -> t1 -> Bool
  rowsAndCols board iter index count player
    | iter > length board = False
    | checkWinSequence (row iter board) player || checkWinSequence (column iter board) player = True
    | otherwise = rowsAndCols board (iter + 1) 0 0 player

  --Counts the diaganol stones per player
  leftAndRightDiag :: (Eq t1, Num t2, Num t3) => [[t1]] -> Int -> t2 -> t3 -> t1 -> Bool
  leftAndRightDiag board iter index count player
    | iter > (length board * 2) - 1  = False
    | checkWinSequence (ldiag iter board) player || checkWinSequence (rdiag iter board) player = True
    | otherwise = leftAndRightDiag board (iter + 1) 0 0 player

  --Checks if the sequence of stones is a win
  checkWinSequence :: Eq t => [t] -> t -> Bool
  checkWinSequence (h : t) player
    | length t < 4 = False
    | h == player && all (h==) (take 4 t) = True
    | otherwise = checkWinSequence t player

  --Check if the board is full
  isDraw :: [[String]] -> Bool
  isDraw = isFull

  --Return game over if conditions are met
  isGameOver :: [[String]] -> Bool
  isGameOver bd = isDraw bd || isWonBy bd "x" || isWonBy bd "o"

  --Return a board in a string form
  boardToStr :: (t -> Char) -> [[t]] -> [Char]
  boardToStr playerToChar bd = " x " ++ topXBoard 0 (size bd) ++ "\n" ++ "y " ++ topYBoard (size bd * 2) ++ "\n" ++ concat (getRows playerToChar 1 bd)
  getRows :: (t -> Char) -> Int -> [[t]] -> [String]
  getRows playerToChar n bd = if n > length bd then [] else show (mod n 10): "|" : paddedRow playerToChar (row n bd) : "\n" : getRows playerToChar (n + 1) bd
  paddedRow :: (t -> Char) -> [t] -> [Char]
  paddedRow playerToChar (h : t) = if null t then " " ++ [playerToChar h] else " " ++ [playerToChar h] ++ paddedRow playerToChar t
  topXBoard :: (Show t, Integral t) => t -> t -> [Char]
  topXBoard n m = if n == m then "" else show (mod (n + 1) 10) ++ " " ++ topXBoard (n + 1) m
  topYBoard :: (Eq t, Num t) => t -> [Char]
  topYBoard n = if n == 0 then "" else "-" ++ topYBoard (n - 1)