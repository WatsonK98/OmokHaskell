import Data.List (transpose)
type Board = [[Char]]
type Player = Char

mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n '.')

mkPlayer :: Player
mkPlayer = 'X'

mkOpponent :: Player
mkOpponent = 'O'

size :: Board -> Int
size bd = length bd

row :: Int -> Board -> [Char]
row y bd = bd !! (y-1)

column :: Int -> Board -> [Char]
column x bd = map (!! (x-1)) bd

mark :: Int -> Int -> Board -> Player -> Board
mark x y bd p = take (y-1) bd ++ [take (x-1) (bd !! (y-1)) ++ [p] ++ drop x (bd !! (y-1))] ++ drop y bd

isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y bd = (bd !! (y-1)) !! (x-1) == '.'

isMarked :: Int -> Int -> Board -> Bool
isMarked x y bd = not (isEmpty x y bd)

isMarkedBy :: Int -> Int -> Board -> Player -> Bool
isMarkedBy x y bd p = (bd !! (y-1)) !! (x-1) == p

marker :: Int -> Int -> Board -> Player
marker x y bd = (bd !! (y-1)) !! (x-1)

isFull :: Board -> Bool
isFull bd = all (\row -> all (\c -> c /= '.') row) bd

diag :: [[a]] -> [a]
diag m = [m !! i !! i | i <- [0..length m - 1]]

isWonBy :: Board -> Char -> Bool
isWonBy bd p = or (map (all (== p)) (rows ++ cols ++ diags))
  where
    rows = bd
    cols = transpose bd
    diags = [diag bd, diag (map reverse bd)]

isDraw :: Board -> Bool
isDraw bd = isFull bd && not (isWonBy bd 'X') && not (isWonBy bd 'O')

isGameOver :: Board -> Bool
isGameOver bd = isDraw bd || isWonBy bd 'X' || isWonBy bd 'O'

boardToStr :: (Int -> Int -> Char) -> [[Int]] -> String
boardToStr playerToChar bd =
  let header = "  x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n"
      line = "y ------------------------------\n"
      rowToString row = '|' : [playerToChar x y | x <- row, let y = head row, y /= -1] ++ "\n"
      boardString = concat [rowToString row | row <- bd]
  in header ++ line ++ boardString