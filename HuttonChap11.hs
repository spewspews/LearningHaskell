import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concatMap (filter (/= B)) g

wins :: Player -> Grid -> Bool
wins p g = any (all (== p)) $ rows ++ cols ++ dias
  where
    rows = g
    cols = transpose g
    dias = [diag g, diag $ map reverse g]

diag :: Grid -> [Player]
diag g = zipWith (!!) g [0 ..]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> Maybe Grid
move g i p =
    if valid g i then Just (chop size (xs ++ [p] ++ ys)) else Nothing
  where
    (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs
        then return $ read xs
        else do
            putStrLn "ERROR: Invalid number"
            getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1, 1)
    putGrid g
    run' g p

run' :: Grid -> Player -> IO ()
run' g p
    | wins O g = putStrLn "Player O Wins!\n"
    | wins X g = putStrLn "Player X Wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | otherwise = do
        i <- getNat (prompt p)
        case move g i p of
            Just g' -> run g' (next p)
            Nothing -> do
                putStrLn "ERROR: Invalid move"
                run' g p

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
