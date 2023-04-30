import Data.List (transpose)

mkBoard :: Int -> [[Char]]
mkBoard n = replicate n (replicate n '.')

mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

size :: [[Char]] -> Int
size = length

row :: Int -> [[Char]] -> [Char]
row y bd = bd !! (y-1)

column :: Int -> [[Char]] -> [Char]
column x = map (!! (x-1))

mark :: Int -> Int -> [[Char]] -> Char -> [[Char]]
mark x y bd p = take (y-1) bd ++ [take (x-1) (bd !! (y-1)) ++ [p] ++ drop x (bd !! (y-1))] ++ drop y bd

isEmpty :: Int -> Int -> [[Char]] -> Bool
isEmpty x y bd = (bd !! (y-1)) !! (x-1) == '.'

isMarked :: Int -> Int -> [[Char]] -> Bool
isMarked x y bd = not (isEmpty x y bd)

isMarkedBy :: Int -> Int -> [[Char]] -> Char -> Bool
isMarkedBy x y bd p = (bd !! (y-1)) !! (x-1) == p

marker :: Int -> Int -> [[Char]] -> Char
marker x y bd = (bd !! (y-1)) !! (x-1)

isFull :: [[Char]] -> Bool
isFull = all (notElem '.')

diag :: [[a]] -> [a]
diag m = [m !! i !! i | i <- [0..length m - 1]]

isWonBy :: [[Char]] -> Char -> Bool
isWonBy bd p = any (all (== p)) (rows ++ cols ++ diags)
  where
    rows = bd
    cols = transpose bd
    diags = [diag bd, diag (map reverse bd)]

isDraw :: [[Char]] -> Bool
isDraw bd = isFull bd && not (isWonBy bd 'X') && not (isWonBy bd 'O')

isGameOver :: [[Char]] -> Bool
isGameOver bd = isDraw bd || isWonBy bd 'X' || isWonBy bd 'O'

boardToStr :: (Int -> Int -> Char) -> [[Int]] -> String
boardToStr playerToChar bd =
  let header = "  x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n"
      line = "y ------------------------------\n"
      rowToString row = '|' : [playerToChar x y | let y = head row, y /= - 1, x <- row] ++ "\n"
      boardString = concat [rowToString row | row <- bd]
  in header ++ line ++ boardString