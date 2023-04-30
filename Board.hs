import Data.List (transpose)
type Board = [[Int]]
type Player = Int

--Create a board that is a 15x15 filled with 0's
mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n 0)

--Indicate the first player
mkPlayer :: Int
mkPlayer = 1

--Indicate the second player
mkOpponent :: Int
mkOpponent = 2

--Returns the board size
size :: Board -> Int
size bd = 15

--Returns a row
row :: Int -> Board -> [Int]
row y bd = bd !! (y-1)

--Returns a Column
column :: Int -> Board -> [Int]
column x = map (!! (x-1))

--Places a player marker 1 or 2 on the board
mark :: Int -> Int -> Board -> Player -> Board
mark x y bd p = take (y-1) bd ++ [take (x-1) (bd !! (y-1)) ++ [p] ++ drop x (bd !! (y-1))] ++ drop y bd

--Checks if the coordinates are empty ie. 0
isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y bd = (bd !! (y-1)) !! (x-1) == 0

--Checks if the coordinate contains anything but 0
isMarked :: Int -> Int -> Board -> Bool
isMarked x y bd = not (isEmpty x y bd)

--Checks if the corrdinates are marked by a player
isMarkedBy :: Int -> Int -> Board -> Player -> Bool
isMarkedBy x y bd p = (bd !! (y-1)) !! (x-1) == p

--Return the player of a marked coordinate
marker :: Int -> Int -> Board -> Player
marker x y bd = (bd !! (y-1)) !! (x-1)

--Checks if the board is full
isFull :: Board -> Bool
isFull = all (notElem 0)

diag :: [[a]] -> [a]
diag m = [m !! i !! i | i <- [0..length m - 1]]

--Checks if game is won by a player
isWonBy :: Board -> Int -> Bool
isWonBy bd p = any (all (== p)) (rows ++ cols ++ diags)
  where
    rows = bd
    cols = transpose bd
    diags = [diag bd, diag (map reverse bd)]

--Checks if game is a draw
isDraw :: Board -> Bool
isDraw bd = isFull bd && not (isWonBy bd 1) && not (isWonBy bd 2)

--Checks if game is over
isGameOver :: Board -> Bool
isGameOver bd = isDraw bd || isWonBy bd 1 || isWonBy bd 2

--Outputs the OMOK board
boardToStr :: (Int -> Int -> Char) -> [[Int]] -> String
boardToStr playerToChar bd =
  let header = "  x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n"
      line = "y ------------------------------\n"
      rowToString row = '|' : [playerToChar x y | let y = head row, y /= - 1, x <- row] ++ "\n"
      boardString = concat [rowToString row | row <- bd]
  in header ++ line ++ boardString