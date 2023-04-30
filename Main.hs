import Board ( mkBoard, size, mark, isEmpty, isFull, boardToStr )

playerToChar :: Int -> Char
playerToChar p
  | p == 1 = 'X'
  | p == 2 = 'O'
  | otherwise = '.'

readXY :: [[Int]] -> Int -> IO (Int, Int)
readXY bd p = do
  putStrLn $ "Player " ++ [playerToChar p] ++ ", enter your move as x,y:"
  line <- getLine
  case reads line of
    [(x, ',':rest)] -> case reads rest of
      [(y, "")] -> if x > 0 && x <= size bd && y > 0 && y <= size bd && isEmpty bd x y
        then return (x, y)
        else putStrLn "Invalid move." >> readXY bd p
      _ -> putStrLn "Invalid input." >> readXY bd p
    _ -> putStrLn "Invalid input." >> readXY bd p

main :: IO ()
main = do
    putStrLn "New Game!"
    let bd = mkBoard 15
    playGame bd 1

playGame :: [[Int]] -> Int -> IO ()
playGame bd p = do
  putStr $ boardToStr bd
  if isWin bd p
    then putStrLn $ "Player " ++ [playerToChar p] ++ " wins!"
    else if isFull bd
      then putStrLn "The game is a draw."
      else do
        putStrLn $ "Player " ++ [playerToChar p] ++ ", make your move."
        (x, y) <- readXY bd p
        let bd' = mark bd p x y
        playGame bd' (otherPlayer p)