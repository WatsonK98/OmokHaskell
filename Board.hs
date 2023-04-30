type Board -> [[Int]]

mkBoard :: Int -> [[Char]]
mkBoard n = replicate n (replicate n '.')

mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

size :: [[a]] -> Int
size bd = length bd

row :: Int -> [[a]] -> [a]
row y bd = bd !! (y-1)

column :: Int -> [[a]] -> [a]
column x bd = map (!! (x-1)) bd

mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
mark x y bd p = take (y-1) bd ++ [take (x-1) row ++ [p] ++ drop x row] ++ drop y bd
  where row = bd !! (y-1)

isEmpty :: Int-> Int -> [[Char]] -> Bool
isEmpty x y bd = bd !! (y-1) !! (x-1) == '.'

isMarked :: Int -> Int -> [[Char]] -> Bool
isMarked x y bd = bd !! (y-1) !! (x-1) /= '.'

ismarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
ismarkedBy x y bd p = bd !! (y-1) !! (x-1) == p

marker :: Int -> Int -> [[Int]] -> Maybe Int
marker x y bd
 | ismarkedBy x y bd 1 = Just 1
 | ismarkedBy x y bd 2 = Just 2
 | otherwise = Nothing

isFull :: [[Char]] -> Bool
isFull bd = all (not . elem '.') bd

countInARow :: Int -> [Int] -> Int
countInARow p xs = maximum $ 0 : [n | ns <- group xs, head ns == p, let n = length ns]

winningRow :: Int -> [Int] -> Bool
winningRow p row = countInARow p row >= 5

hasWinningRow :: [[Int]] -> Int -> Bool
hasWinningRow bd p = any (winningRow p) bd
 where
    winningRow p row = countInARow p row >= 5
    countInARow p [] = 0
    countInARow p (x:xs)
     | x == p = 1 + countInARow p xs
     | otherwise = 0 + countInARow p xs

isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = any (hasWinningRow p) (rows bd ++ cols bd ++ diags bd)
 where
    rows = id
    cols = transpose
    diags bd = [diag, diag . map reverse]
     where diag = [bd !! i !! i | i <= [0..length bd - 1]]

isDraw :: Board -> Bool
isDraw bd = all (not . isEmptyPlace bd) [(x,y) | x <- [1..size bd], y <- [1..size bd]]

isGameOver :: Board -> Bool
isGameOver bd = isFull bd || isWonBy bd 1 || isWonBy bd 2

boardToStr :: (Player -> Char) -> Board -> String
boardToStr playertoChar bd = unlines $ header : seperator : body
 where
    header = " x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5"
    separator = " y ---------------------------"
    body = [ rowToStr y | y <- [boardHeigh, boardHeigh -1 .. 1] ]
    rowToStr y = intToDigit y : "| " ++ [ playerToChar (bd get (x, y)) | x <- [1..boardWidth]] ++ " |"