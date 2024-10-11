import qualified Data.IntSet as S (IntSet, insert, member)
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

match :: Regex -> String -> Bool
match r s = go (0 `cons` empty) empty $ s ++ ['\NUL']
  where
    go (UL _ []) next (_ : cs) = go (0 `cons` next) empty cs
    go (UL opset (op : ops)) next s@(c : _) = case r V.! op of
      DOT -> go tl (op + 1 `cons` next) s
      CHAR c' ->
        if c == c'
          then go tl (op + 1 `cons` next) s
          else go tl next s
      FORK j k -> go (op + j `cons` op + k `cons` tl) next s
      END -> True
      where
        tl = UL opset ops
    go _ _ "" = False
