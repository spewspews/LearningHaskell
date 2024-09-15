import Data.IntSet (IntSet, insert, member)
import Data.Vector (Vector, fromList, (!))

data Op
    = DOT
    | CHAR Char
    | FORK Int Int
    | END
    deriving Show

type Regex = Vector Op

parse :: String -> Either String Regex
parse s = fromList . reverse . (END :) . fst <$> go [[]] s
  where
    go opss "" = Right (concat opss, "")
    go (prevOps : opss) (c : cs) = case c of
        '.' -> go ([DOT] : prevOps : opss) cs
        '*' -> go (([FORK (-l) 1] ++ prevOps ++ [FORK 1 (l + 2)]) : opss) cs
        '+' -> go ([FORK (-l) 1] : prevOps : opss) cs
        '?' -> go ((prevOps ++ [FORK 1 (l + 1)]) : opss) cs
        '(' -> do
            (ops, s) <- go [[]] cs
            case s of
                ')' : cs -> go (ops : prevOps : opss) cs
                _ -> Left "Bad regular expression: missing closing parenthesis"
        ')' -> Right (concat $ prevOps : opss, c : cs)
        '\\' -> case cs of
            c : cs -> go ([CHAR c] : prevOps : opss) cs
            _ -> Left "Bad regular expression: escape sequence"
--        '|' -> go ([FORK 1 (WHAT LENGTH GOES HERE)] : prevOps : opss ++ [[FORK 1 (l + length (concat opss) + 2)]]) cs
        c -> go ([CHAR c] : prevOps : opss) cs
      where
        l = length prevOps

data State = State {s :: IntSet, ops :: [Int]} deriving Show

empty :: State
empty = State mempty []

add :: Int -> State -> State
add i st@(State s l) =
    if member i s then st else State (insert i s) (i : l)

match :: Regex -> String -> Bool
match r s = go (add 0 empty) empty $ s ++ ['\NUL']
  where
    go (State _ []) next (_ : cs) = go (add 0 next) empty cs
    go cur@(State _ (op : ops)) next s@(c : _) = case r ! op of
        DOT -> go cur{ops} (add (op + 1) next) s
        CHAR c' ->
            if c == c'
                then go cur{ops} (add (op + 1) next) s
                else go cur{ops} next s
        FORK j k -> go (add (op + j) $ add (op + k) cur{ops}) next s
        END -> True
    go _ _ "" = False
