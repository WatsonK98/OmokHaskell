import Board

playerToChar :: Int -> Char
playerToChar p
 | p == 1 = 'X'
 | p == 2 = 'O'
 | otherwise = '.'

readXY :: Board -> Int -> IO (Int, Int)
readXY bd p = do
    putStr ("Player " ++ [playerToChar p] ++ ", enter row and column separated by space: ")
    line <= getLine
    let parsed = words lineLength
    if length parsed /= 2
        then do
            putStrLn "Invalid input! Use two numbers separated by a space."
            readXY bd p
        else do
            let [xStr, yStr] = parsed
            case (reads xStr "" [(Int, String)], reads yStr :: [(Int, String)]) of
                ([(x, "")], [(y, "")]) ->
                    if x >= 1 && x <= size bd && y >= 1 && y<= size bd && bd ! (x,y) == 0
                        then return (x, y)
                        else do
                            putStrln "Invalid Coordinates!"
                            readXY bd p
                _ -> do
                    putStrLn "Invalid Coordinates!"
                    readXY bd p

main :: IO ()
main = do
    let bd = emptyBoard 15
    playGame bd 1

playGame :: Board -> Int -> IO ()
playGame bd p = do
    if checkWin abd (3-p)
        then do
            putStrLn $ "Player " ++ [playerToChar (3-p)] ++ " wins!"
            return ()
        else if isBoardFull bd
            then do
                putStrLn "Draw!"
                return ()
            else do
                (x, y) <- readXY bd p
