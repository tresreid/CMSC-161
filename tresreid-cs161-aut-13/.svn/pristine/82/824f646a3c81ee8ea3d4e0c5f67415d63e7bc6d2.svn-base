module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB


TEST
> test initial
True
look at bottom to see the test function. 
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- TODO: uncomment these type declarations and implement the functions
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
--TODO: replace below line with real definition
getCurPlayer (MancalaBoardImpl list player) = player


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
--TODO: replace below line with real definition
getBoardData (MancalaBoardImpl list player) = list


{- return the side of the board for a specified player, including the store at
 - the end -}
--TODO: define this function
playerSide :: MancalaBoard -> [Int]
playerSide board = (indicesForPlayerSide (getCurPlayer board)) ++ ((indexForPlayerStore (getCurPlayer board)):[])


{- return the number of captured pieces in specified player's store -}
--TODO: add type and replace below line with real definition
numCaptured :: MancalaBoard-> Player -> Int
numCaptured board player = (getBoardData board) !! (indexForPlayerStore player)


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
--TODO: add type and replace below line with real definition
allowedMoves :: MancalaBoard -> [Int]
allowedMoves board = canDo 0 []where
    canDo 6 y = reverse y {- to get the right orientation because it adds in the first ones first so it is pushed to the back....right?-}
    canDo x y
        | ((playerSide board !! x) == 0) = canDo (x+1) y
        | otherwise = canDo (x+1) (x:y)


{- check that a move is valid for the current player -}
--TODO: add type and replace below line with real definition
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove board tileNum
    | ((tileNum `elem` (allowedMoves board)) == True) = True
    | otherwise = False


{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
--TODO: add type and replace below line with real definition
move :: MancalaBoard -> Int -> MancalaBoard
move board i = MancalaBoardImpl newBoard newPlayer where
                     board' = fst $ takeStones (getBoardData board) i
                     newPlayer = if ((i + (getBoardData board) !! i) == indexForPlayerStore (getCurPlayer board)) then (getCurPlayer board) else (nextPlayer (getCurPlayer board))

                     newBoard = if (isAllowedMove board i == True) then (movetiles board' ((getBoardData board) !! i) ((i+1) `mod` (length $ getBoardData board)) (indexForPlayerStore (nextPlayer (getCurPlayer board)))) else (error "not allowed move")

movetiles :: [Int] -> Int -> Int -> Int -> [Int]
movetiles board 0 _ _ = board
movetiles board stones i store 
    | i == store = movetiles board stones ((i+1) `mod` (length board)) store
    | otherwise = movetiles (changeBoard board i) (stones -1) ((i+1) `mod` (length board)) store

takeStones :: [Int] -> Int -> ([Int],Int)
takeStones board 0 = ((0 : (tail board)), head board)
takeStones board i = ((( head board) : (fst tup)), snd tup) where tup = takeStones (tail board) (i-1)
changeBoard board i 
    | i> length board = changeBoard board (i `mod` length board)
    | i == 0 = (( head board +1) : (tail board))
    | otherwise = (head board) : (changeBoard (tail board) (i-1))
{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- TODO: replace below line with real definition
gameOver (MancalaBoardImpl list player)
    | (allowedMoves (MancalaBoardImpl list player) /= []) = False
    | (allowedMoves (MancalaBoardImpl list (nextPlayer player)) /= []) = False
    | otherwise = True 


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners board 
    | (numCaptured board PlayerA < numCaptured board PlayerB) = [PlayerB]
    | (numCaptured board PlayerA > numCaptured board PlayerB) = [PlayerA]
    | (numCaptured board PlayerA == numCaptured board PlayerB) = allPlayers


---- show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            (show (take 7 boardData)) ++ " " ++ (show player) ++ "\n"++ (show (drop 7boardData)) ++ " " ++ (show $ nextPlayer player)
test :: MancalaBoard -> Bool -- checks to see if the functions work at the initial state
test board
    |(getCurPlayer board == PlayerA) && (getBoardData board == [4,4,4,4,4,4,0,4,4,4,4,4,4,0]) && (playerSide board == [0,1,2,3,4,5,6]) && (numCaptured board PlayerA == 0) && (allowedMoves board == [1,2,3,4,5]) && (isAllowedMove board 1 == True) && (isAllowedMove board 6 == False) && (gameOver board == False) && ( getBoardData (move initial 3) == [4,4,4,0,5,5,1,5,4,4,4,4,4,0]) = True
    |otherwise = False
