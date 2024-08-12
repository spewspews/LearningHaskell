import Data.Vector (Vector, fromList, (!))

data Op
  = DOT
  | CHAR Char
  | FORK Int Int
  | END
  deriving (Show, Ord, Eq, Read)

type Regex = Vector Op

parse :: String -> Regex
parse = fromList . reverse . (END :) . fst . go ([], 0)
  where
    go (ops, i) "" = (ops, "")
    go (ops, i) (c : cs) = case c of
      '.' -> go (DOT : ops, 1) cs
      '*' ->
        go (FORK (-i) 1 : take i ops ++ FORK 1 (i + 2) : drop i ops, i + 2) cs
      '+' -> go (FORK (-i) 1 : ops, i + 1) cs
      '?' ->
        go (take i ops ++ FORK 1 (i + 1) : drop i ops, i + 1) cs
      '(' -> case s' of
        (')' : cs') -> go (ops' ++ ops, length ops') cs'
        _ -> error "Bad Regular Expression"
        where
          (ops', s') = go ([], 0) cs
      ')' -> (ops, c : cs)
      c -> go (CHAR c : ops, 1) cs

match :: Regex -> String -> Bool
match r s = go [0] [] $ s ++ ['\NUL']
  where
    go [] next (_ : cs) = go (0 : next) [] cs
    go (i : ops) next s@(c : _) = case r ! i of
      DOT -> go ops (i + 1 : next) s
      CHAR c' -> if c == c' then go ops (i + 1 : next) s else go ops next s
      FORK f g -> go (i + f : i + g : ops) next s
      END -> True
    go cur _ [] = False
