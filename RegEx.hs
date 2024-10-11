import Data.Vector (Vector, fromList, (!))

data Op
  = DOT
  | CHAR Char
  | FORK Int Int
  | END
  deriving (Show, Ord, Eq, Read)

type Regex = Vector Op

parse :: String -> Regex
parse = fromList . reverse . (END :) . concat . fst . go [[]]
  where
    go ops "" = (ops, "")
    go ops (c : cs) = case c of
      '.' -> go ([DOT] : ops) cs
      '*' -> go (([FORK (-l) 1] ++ op' ++ [FORK 1 (l + 2)]) : ops') cs
        where
          op' : ops' = ops
          l = length op'
      '+' -> go ([FORK (-l) 1] : ops) cs
        where
          l = length $ head ops
      '?' -> go ((op' ++ [FORK 1 (l + 1)]) : ops') cs
        where
          op' : ops' = ops
          l = length op'
      '(' -> case s' of
        (')' : cs') -> go (concat ops' : ops) cs'
        _ -> error "Bad Regular Expression"
        where
          (ops', s') = go [[]] cs
      ')' -> (ops, c : cs)
      c -> go ([CHAR c] : ops) cs

match :: Regex -> String -> Bool
match r s = go [0] [] $ s ++ ['\NUL']
  where
    go [] next (_ : cs) = go (0 : next) [] cs
    go (i : ops) next s@(c : _) = case r ! i of
      DOT -> go ops (i + 1 : next) s
      CHAR c' -> if c == c' then go ops (i + 1 : next) s else go ops next s
      FORK f g -> go (i + f : i + g : ops) next s
      END -> True
    go _ _ "" = False
