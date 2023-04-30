import Board

playerToChar :: Int -> Char
playerToChar p
    | p == mkPlayer = 'O'
    | p == mkOpponent = 'X'
    | otherwise = '.'