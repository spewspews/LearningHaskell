import qualified Data.IntSet as S (IntSet, fromList, insert, member, toList)
import qualified Data.Vector as V (Vector, fromList, (!))

data Op
  = DOT
  | CHAR Char
  | FORK Int Int
  | END
  deriving (Show, Ord, Eq, Read)

type Regex = V.Vector Op

parse :: String -> Regex
parse = V.fromList . reverse . (END :) . fst . go [[]]
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

data UL = UL S.IntSet [Int]

empty :: UL
empty = UL mempty []

cons :: Int -> UL -> UL
cons i ul@(UL s l) =
  if S.member i s then ul else UL (S.insert i s) (i : l)

infixr 5 `cons`

fromList :: [Int] -> UL
fromList l = UL s (S.toList s) where s = S.fromList l

match :: Regex -> String -> Bool
match r s = go (fromList [0]) empty $ s ++ ['\NUL']
  where
    go (UL _ []) next (_ : cs) = go (0 `cons` next) empty cs
    go (UL is (op : ops)) next s@(c : _) = case r V.! op of
      DOT -> go (UL is ops) (op + 1 `cons` next) s
      CHAR c' -> if c == c' then go (UL is ops) (op + 1 `cons` next) s else go (UL is ops) next s
      FORK j k -> go (op + j `cons` op + k `cons` UL is ops) next s
      END -> True
    go _ _ "" = False
