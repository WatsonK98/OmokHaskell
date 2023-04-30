import Board ()

playerToChar :: Int -> Char
playerToChar p
  | p == 1 = 'X'
  | p == 2 = 'O'
  | otherwise = '.'


readXY :: Board -> Int -> IO (Int, Int)
readXY bd p = do
  putStrLn $ "Player " ++ show p ++ ", enter your move as x,y:"
  line <- getLine
  case reads line of
    [(x, ',':rest)] -> case reads rest of
      [(y, "")] -> if x > 0 && x <= width bd && y > 0 && y <= height bd && isEmpty x y bd
        then return (x, y)
        else putStrLn "Invalid move." >> readXY bd p
      _ -> putStrLn "Invalid input." >> readXY bd p
    _ -> putStrLn "Invalid input." >> readXY bd p

main :: IO ()
main = do
    putStrLn "New Game!"
    let bd = mkboard 15
    playGame bd X

playGame :: Board -> Int -> IO ()
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
    

