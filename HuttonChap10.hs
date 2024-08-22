import Control.Monad (zipWithM_)
import Data.Char (digitToInt, isDigit)
import System.IO (hSetEcho, stdin)

act :: IO (Char, Char)
act = do
    x <- getChar
    getChar
    y <- getChar
    return (x, y)

getLine' :: IO String
getLine' = do
    c <- getChar
    if c == '\n'
        then return []
        else do
            s <- getLine'
            return $ c : s

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (c : cs) = do
    putChar c
    putStr' cs

putStrLn' :: String -> IO ()
putStrLn' s = do
    putStr' s
    putChar '\n'

strlen :: IO ()
strlen = do
    putStr "Give me a string: "
    s <- getLine
    putStr "It has "
    putStr $ show $ length s
    putStrLn " characters"

-- Hangman
hangman :: IO ()
hangman = do
    putStrLn "Think of a word:"
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n'
        then do
            putChar x
            return []
        else do
            putChar '-'
            xs <- sgetLine
            return (x : xs)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word
        then putStrLn "You got it!!"
        else do
            putStrLn $ match word guess
            play word

match :: String -> String -> String
match word guess = map (\c -> if c `elem` guess then c else '-') word

-- Nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row stars = stars <= board !! (row - 1)

move :: Board -> Int -> Int -> Board
move board row stars = zipWith update [1 ..] board
  where
    update r n = if r == row then n - stars else n

putRow :: Int -> Int -> IO ()
putRow row stars = do
    putStr $ show row
    putStr ": "
    putStrLn $ unwords $ replicate stars "*"

putBoard :: Board -> IO ()
putBoard = zipWithM_ putRow [1 ..]

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    x <- getChar
    newline
    if isDigit x
        then return $ digitToInt x
        else do
            putStrLn "ERROR: Invalid digit"
            getDigit prompt

playNim :: Board -> Int -> IO ()
playNim board player = do
    newline
    putBoard board
    if finished board
        then do
            newline
            putStr "Player "
            putStr $ show $ next player
            putStrLn " wins!!"
        else do
            newline
            putStr "Player "
            print player
            row <- getDigit "Enter a row number: "
            stars <- getDigit "Stars to remove: "
            if valid board row stars
                then playNim (move board row stars) $ next player
                else do
                    newline
                    putStrLn "ERROR: Invalid move"
                    playNim board player

nim :: IO ()
nim = play initial 1
