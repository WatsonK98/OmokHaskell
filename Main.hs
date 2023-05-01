--Alexander Watson and Nathanael Perez
--CS 3360
--Dr. Cheon
--Main Module

module Main where

  import Board
  import System.IO
  import System.Exit
  import Control.Monad

  --Return the difference of the players entries
  playerToChar :: String -> Char
  playerToChar p
    | p == "1" = 'x'
    | p == "2" = 'o'
    | otherwise = '.'

  --Check if the selected space is empty
  readyXY :: (Num a, Num b) => [[String]] -> [Char] -> IO (a, b)
  readyXY bd p = do
    putStr ("Player " ++ p ++ " turn: ")
    (x,y) <- getXY
    if isEmpty (fromIntegral x) (fromIntegral y) bd
    then return (fromIntegral x, fromIntegral y)
    else do
      putStrLn "Place is not empty"
      readyXY bd p

  --Return the x and y coordinates that the player inputed
  getXY :: IO (Int, Int)
  getXY = do
    putStrLn "Enter x and y (eg. 8 10 or -1 to quit)?"
    (x,y) <- readValidInput
    return (x,y)

  --Checks if the player wants to end the game, checks that the input has two integers separated by a " "
  readValidInput :: IO (Int, Int)
  readValidInput = do
    line <- getLine
    if line == "-1" then do endGame
      else do
        let inputList = words line
        if length inputList /= 2
          then do
          putStrLn "Invalid input. Please enter two integers separated by a space, or enter -1 to quit:"
          readValidInput
        else do
          let [x, y] = map parseMaybeInt inputList
          case (x, y) of
            (Just x', Just y')
              | isValidInput x' y' -> return (x', y')
              | otherwise -> do
                  putStrLn "Invalid input."
                  getXY
            _ -> do
              putStrLn "Invalid input."
              getXY

  --Just the definitions of the previous functions
  isValidInput :: Int -> Int -> Bool
  isValidInput x y = x > 0 && x < 16 && y > 0 && y < 16
  parseMaybeInt :: String -> Maybe Int
  parseMaybeInt s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

  --Starts the main program
  main ::IO()
  main = do
    putStrLn "Welcome to Omok Game"
    let bd = mkBoard 15
    runGame bd

 --Game that carries a Player VS Player
  runGame :: [[String]] -> IO b
  runGame bd = do
    putStr (boardToStr playerToChar bd)
    tuplePlayer1 <- readyXY bd mkPlayer
    let player1MoveBd = uncurry mark tuplePlayer1 bd mkPlayer
    putStr (boardToStr playerToChar player1MoveBd)
    checkIfWon player1MoveBd mkPlayer
    checkIfDraw player1MoveBd
    tuplePlayer2 <- readyXY player1MoveBd mkOpponent
    let player2MoveBd = uncurry mark tuplePlayer2 player1MoveBd mkOpponent
    checkIfWon player2MoveBd mkOpponent
    checkIfDraw player2MoveBd
    runGame player2MoveBd

  --Check if the player won
  checkIfWon :: [[[Char]]] -> [Char] -> IO ()
  checkIfWon bd p = when (isWonBy bd p) $ do
    putStrLn $ "Player " ++ p ++ " won."
    main

  --Check if its a draw
  checkIfDraw :: [[String]] -> IO ()
  checkIfDraw bd = when (isDraw bd) $ do
    putStrLn "Draw."
    main

  --Ends the Game
  endGame :: IO b
  endGame = do
    putStrLn "Game Over"
    exitSuccess