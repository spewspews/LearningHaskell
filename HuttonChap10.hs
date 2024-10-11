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
