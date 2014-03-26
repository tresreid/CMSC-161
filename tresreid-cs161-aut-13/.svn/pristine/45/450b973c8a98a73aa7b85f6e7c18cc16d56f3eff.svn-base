import MancalaBoard
import System.IO

getInt :: IO Int
getInt = readLn

isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove board tileNum
    | ((tileNum `elem` (allowedMoves board)) == True) = True
    | otherwise = False
gameOver :: MancalaBoard -> Bool
-- TODO: replace below line with real definition
gameOver board
    | (allowedMovesFor board (getCurPlayer board) == []) = True
    | (allowedMovesFor board (nextPlayer $ getCurPlayer board) == []) = True
    | otherwise = False

endGame :: MancalaBoard -> IO()
endGame board = do
    putStr "Player "
    putStr (show $whoWins board)
    putStrLn " has won!"

playGame board = do
                 putStrLn (show $ board)
                 putStrLn "show me your moves:"
                 tile <- getInt
                 let tile' = isAllowedMove board tile
                 case tile' of
                     True  ->  do
                               let board' = move board tile
                               nextTurn board'
                     False -> do
                              putStrLn "Try again"
                              playGame board
                       
nextTurn board'
    | (gameOver board' == True) = endGame board'
    | otherwise = playGame board'

main :: IO()
main = do
    putStrLn "Come On and Slam, and Welcome to the Jam!"
    putStrLn "HEY YOU, Whatcha gonna do? When it is your turn, type in a number to move the stones in that pit. If the last stone fall in your storage pit, you get to go again. Try to win."
    let board = initial
    playGame board