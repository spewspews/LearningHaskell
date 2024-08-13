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
        (')' : cs) -> go (ops : prevOps : opss) cs
        _ -> error "Bad Regular Expression"
        where
          (ops, s) = go [[]] cs
      ')' -> (concat (prevOps : opss), c : cs)
      c -> go ([CHAR c] : prevOps : opss) cs
      where
        l = length prevOps

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
