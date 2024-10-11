import Data.Char
import Data.List
import Data.Maybe (mapMaybe)
import System.IO
import System.Random.Stateful (globalStdGen, uniformRM)

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

data Tree a = Node a [Tree a] deriving (Show, Foldable)

gametree :: Grid -> Player -> Tree (Grid, Player)
gametree g p = Node (g, p) [gametree g' (next p) | g' <- moves g p]

getChild :: Tree (Grid, Player) -> Grid -> Tree (Grid, Player)
getChild (Node _ ts) g = head $ filter (\(Node (g', _) _) -> g' == g) ts

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | otherwise = mapMaybe (\i -> move g i p) [0 .. size ^ 2 - 1]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x $ map (prune (n - 1)) ts

depth :: Int
depth = 9

minimax :: Tree (Grid, Player) -> Tree (Grid, Player)
minimax (Node (g, _) [])
    | wins O g = Node (g, O) []
    | wins X g = Node (g, X) []
    | otherwise = Node (g, B) []
minimax (Node (g, p) ts)
    | p == O = Node (g, minimum ps) ts'
    | p == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = map (\(Node (_, p) _) -> p) ts'

bestmoves :: Tree (Grid, Player) -> [Tree (Grid, Player)]
bestmoves (Node (g, best) ts) = filter (\(Node (_, p) _) -> p == best) ts

maxDepth :: Tree a -> Int
maxDepth (Node _ []) = 0
maxDepth (Node _ ts) = 1 + maximum (map maxDepth ts)

minDepth :: Tree a -> Int
minDepth (Node _ []) = 0
minDepth (Node _ ts) = 1 + minimum (map minDepth ts)

getRandom :: [a] -> IO a
getRandom l = do
    x <- uniformRM (0, length l - 1) globalStdGen
    return $ l !! x

randomMove :: Grid -> Player -> IO Grid
randomMove g p = do
    i <- uniformRM (0, size ^ 2 - 1) globalStdGen
    case move g i p of
        Just g -> return g
        Nothing -> randomMove g p

main :: IO ()
{-
main = do
    putStr "Nodes: "
    print $ length gt
    putStr "Depth: "
    print $ maxDepth gt
  where
    gt = gametree empty O
-}
main = do
    hSetBuffering stdout NoBuffering
    putStr "Go first? [yn]: "
    s <- getLine
    if s == "y" then play (minimax $ gametree empty O) O else play (minimax $ gametree empty X) X

play :: Tree (Grid, Player) -> Player -> IO ()
play gt@(Node (g, _) _) p = do
    cls
    goto (1, 1)
    putGrid g
    play' gt p

play' :: Tree (Grid, Player) -> Player -> IO ()
play' gt@(Node (g, _) _) p
    | wins O g = putStrLn "Player O Wins!\n"
    | wins X g = putStrLn "Player X Wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | p == O = do
        i <- getNat (prompt p)
        case move g i p of
            Just g' -> play (getChild gt g') (next p)
            Nothing -> do
                putStrLn "ERROR: Invalid move"
                play' gt p
    | p == X = do
        putStr "Player X is thinking... "
        g' <- getRandom $ bestmoves gt
        play g' (next p)
