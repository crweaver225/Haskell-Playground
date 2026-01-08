{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import Data.List (intercalate, findIndex)
import Data.Maybe (isJust, fromJust, isNothing)
import System.IO (hFlush, stdout)

data Player = X | O deriving (Eq, Show)
type Cell = Maybe Player
type Board = [Cell]

-- initial empty board
emptyBoard :: Board
emptyBoard = replicate 9 Nothing

showCell :: Cell -> Char
showCell Nothing = ' '
showCell (Just X) = 'X'
showCell (Just O) = 'O'

showRow :: Board -> Int -> String
showRow board index = " " ++ intercalate " | " (map (\c -> [showCell c]) cells) ++ " "
    where
        cells = take 3 $ drop (3*index) board

showBoard :: Board -> String
showBoard board =
    unlines [
        showRow board 0,
        "---+---+---",
        showRow board 1,
        "---+---+---",
        showRow board 2
    ]

-- mapping board indices for user reference:
boardKey :: String
boardKey = unlines
  [ " 1 | 2 | 3 "
  , "---+---+---"
  , " 4 | 5 | 6 "
  , "---+---+---"
  , " 7 | 8 | 9 "
  ]

winLines :: [[Int]]
winLines =
    [[0,1,2],[3,4,5],[6,7,8] -- rows
    ,[0,3,6],[1,4,7],[2,5,8] -- cols
    ,[0,4,8],[2,4,6] --diag
    ]

isFull :: Board -> Bool
isFull = notElem Nothing

availableMoves :: Board -> [Int] 
availableMoves board = [i | (i,c) <- zip[0..] board, isNothing c]

checkForWinner :: Board -> Maybe Player
checkForWinner board = findWinner winLines
    where
        findWinner [] = Nothing
        findWinner (l:ls) = case map (board !!) l of
            [Just p1, Just p2, Just p3] | p1 == p2 && p2 == p3 -> Just p1
            _ -> findWinner ls

currentPlayer :: Board -> Player
currentPlayer board = if xs <= os then X else O
    where
        xs = length $ filter (== Just X) board
        os = length $ filter (== Just O) board

makeMove :: Board -> Int -> Player -> Maybe Board
makeMove board index player
    | index < 0 || index >= 9 = Nothing
    | isJust (board !! index) = Nothing
    | otherwise               = Just $ take index board ++ [Just player] ++ drop (index + 1) board

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

parseMove :: String -> Maybe Int
parseMove s =
    case reads s :: [(Int,String)] of
    [(n,"")] | n >= 1 && n <= 9 -> Just (n-1)  -- user enters 1..9
    _ -> Nothing

score :: Maybe Player -> Int 
score (Just X) = 1
score (Just O) = -1
score Nothing = 0

miniMax :: Board -> Player -> (Int, Maybe Int) -- (Score, Move)
miniMax board aiPlayer = 
    case checkForWinner board of 
        Just p -> (score (Just p), Nothing)
        Nothing | isFull board -> (0, Nothing)
        _ -> bestChoice movesWithScores -- Get winning move from list of (Score,Move)
    where 
        moves = availableMoves board -- Get any move the player can legally make
        movesWithScores = [(idx, evaluate idx) | idx <- moves] -- For each move (idx), evaluate if you can win 
        evaluate idx = 
            let p = currentPlayer board 
                Just board' = makeMove board idx p -- Simulate the move
                (s, _) = miniMax board' aiPlayer -- Simulate the game to see if user will win or lose with this move
            in s -- Return if win or loss for this specific move
        bestChoice mws
            | currentPlayer board == X =
                let (m, s) = maximumByScore mws
                in (s, Just m)
            | otherwise =
                let (m, s) = minimumByScore mws
                in (s, Just m)
        maximumByScore = foldr1 (\a@(_,sa) b@(_,sb) -> if sa >= sb then a else b)
        minimumByScore = foldr1  (\a@(_,sa) b@(_,sb) -> if sa >= sb then b else a)


makeAIMove :: Board -> Player -> Int 
makeAIMove board ai = 
    case miniMax board ai of 
        (_, Just idx) -> idx
        (_, Nothing) -> head (availableMoves board) -- fallback

gameLoop :: Board -> Maybe Player -> IO ()
gameLoop board aiPlayerM = do
    putStrLn $ showBoard board
    case checkForWinner board of
        (Just p) -> do
            putStrLn $ "Player " ++ show p ++ " Wins!"
        Nothing | isFull board -> putStrLn "It's a draw!"
        Nothing -> do
            let player = currentPlayer board
            putStrLn $ "Current: " ++ show player
            moveIndex <- if humanTurn player 
                        then humanMove board
                        else aiMove player
            case makeMove board moveIndex player of
                Nothing -> do
                    putStrLn "Invalid move (cell taken or out of range). Try again."
                    gameLoop board aiPlayerM
                Just board' -> gameLoop board' aiPlayerM
    where
        humanTurn p = do 
            case aiPlayerM of 
                Nothing -> True 
                (Just ai) -> ai /= p
        humanMove board = do
            putStrLn "Enter move (1-9). Board positions:"
            putStrLn boardKey
            move <- prompt "> "
            case parseMove move of
                Just idx -> return idx
                Nothing -> putStrLn "Invalid input. Please enter 1..9." >> humanMove board
        aiMove p = do
            putStrLn  "AI thinking.."
            let ai = fromJust aiPlayerM
            let idx = makeAIMove board p
            putStrLn  $ "AI chooses " ++ show (idx + 1)
            return idx

main :: IO ()
main = do
    mode <- prompt "Choose mode: (1) Human vs Human, (2) Human vs AI :"
    case mode of
        "1" -> do
            putStrLn "Human vs Human. X goes first"
            gameLoop emptyBoard Nothing
        "2" -> do
            who <- prompt "Play as X or O? (X goes first)"
            case who of 
                "X" -> do 
                    putStrLn "You are X. AI is O. You go first"
                    gameLoop emptyBoard (Just O)
                "O" -> do 
                    putStrLn "You Are O. AI is X. AI goes first."
                    gameLoop emptyBoard (Just X)
                _ -> putStrLn "Invalid mode. exiting.."

        _ -> putStrLn "Invalid choice"

