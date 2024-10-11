import Data.IntSet (IntSet, insert, member)
import Data.Vector (Vector, fromList, (!))

data Op
  = DOT
  | CHAR Char
  | FORK Int Int
  | END
  deriving (Show, Ord, Eq, Read)

type Regex = Vector Op

parse :: String -> Regex
parse = fromList . reverse . (END :) . fst . go [[]]
  where
    go opss "" = (concat opss, "")
    go (prevOps : opss) (c : cs) = case c of
      '.' -> go ([DOT] : prevOps : opss) cs
      '*' -> go (([FORK (-l) 1] ++ prevOps ++ [FORK 1 (l + 2)]) : opss) cs
      '+' -> go ([FORK (-l) 1] : prevOps : opss) cs
      '?' -> go ((prevOps ++ [FORK 1 (l + 1)]) : opss) cs
      '(' -> case s of
        ')' : cs -> go (ops : prevOps : opss) cs
        _ -> error "Bad regular expression: missing closing parenthesis"
        where
          (ops, s) = go [[]] cs
      ')' -> (concat $ prevOps : opss, c : cs)
      '\\' -> case cs of
        c : cs -> go ([CHAR c] : prevOps : opss) cs
        _ -> error "Bad regular expression: escape sequence"
      c -> go ([CHAR c] : prevOps : opss) cs
      where
        l = length prevOps

data State = State {s :: IntSet, ops :: [Int]} deriving (Show)

empty :: State
empty = State mempty []

cons :: Int -> State -> State
cons i ul@(State s l) =
  if member i s then ul else State (insert i s) (i : l)

infixr 5 `cons`

match :: Regex -> String -> Bool
match r s = go (0 `cons` empty) empty $ s ++ ['\NUL']
  where
    go State {ops = []} next (_ : cs) = go (0 `cons` next) empty cs
    go cur@State {ops = op : ops} next s@(c : _) = case r ! op of
      DOT -> go cur {ops} (op + 1 `cons` next) s
      CHAR c' ->
        if c == c'
          then go cur {ops} (op + 1 `cons` next) s
          else go cur {ops} next s
      FORK j k -> go (op + j `cons` op + k `cons` cur {ops}) next s
      END -> True
    go _ _ "" = False
