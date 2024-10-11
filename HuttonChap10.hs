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
            putStrLn (match word guess)
            play word

match :: String -> String -> String
match word guess = map (\c -> if c `elem` guess then c else '-') word
