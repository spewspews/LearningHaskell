import Control.Concurrent (threadDelay)
import Control.Monad (zipWithM_)
import Data.Char (digitToInt, isDigit)
import Data.List (nub)
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
{-
putStr' [] = return ()
putStr' (c : cs) = do
    putChar c
    putStr' cs
-}
putStr' = mapM_ putChar

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

type NimBoard = [Int]

initial :: NimBoard
initial = [5, 4, 3, 2, 1]

finished :: NimBoard -> Bool
finished = all (== 0)

valid :: NimBoard -> Int -> Int -> Bool
valid board row stars = stars <= board !! (row - 1)

move :: NimBoard -> Int -> Int -> NimBoard
move board row stars = zipWith update [1 ..] board
  where
    update r n = if r == row then n - stars else n

putRow :: Int -> Int -> IO ()
putRow row stars = do
    putStr $ show row
    putStr ": "
    putStrLn $ unwords $ replicate stars "*"

putBoard :: NimBoard -> IO ()
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

playNim :: NimBoard -> Int -> IO ()
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
nim = playNim initial 1

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells = mapM_ (`writeat` "O")

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Pos -> [Pos]
neighbs (x, y) =
    map
        wrap
        [ (x - 1, y - 1)
        , (x, y - 1)
        , (x + 1, y - 1)
        , (x - 1, y)
        , (x + 1, y)
        , (x - 1, y + 1)
        , (x, y + 1)
        , (x + 1, y + 1)
        ]

wrap :: Pos -> Pos
wrap (x, y) =
    ( (x - 1) `mod` width + 1
    , (y - 1) `mod` height + 1
    )

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

births :: Board -> [Pos]
births b =
    [ p
    | p <- nub $ concatMap neighbs b
    , isEmpty b p
    , liveneighbs b p == 3
    ]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
    cls
    showcells b
    threadDelay 100000
    life $ nextgen b

-- Exercise 1-3 done above
--
-- Exercise 4.
adder :: IO ()
adder = do
    n <- getDigit "How many numbers? "
    s <- sum <$> mapM (\_ -> getDigit "") [1 .. n]
    putStr "The total is "
    print s

-- Exercise 5.
readLine :: IO String
readLine = go ""
  where
    go s = do
        c <- getCh
        case c of
            '\n' -> do
                putChar '\n'
                return $ reverse s
            '\DEL' -> case s of
                "" -> go []
                (x : xs) -> do
                    putChar '\b'
                    go xs
            _ -> do
                putChar c
                go (c : s)

{-
readLine = do
    c <- getCh
    case c of
        '\n' -> return []
        '\DEL' -> do
            putChar '\b'
            readLine
        _ -> do
            putChar c
            s <- readLine
            return (c : s)
-}
