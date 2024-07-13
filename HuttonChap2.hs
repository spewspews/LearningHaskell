-- Exercise 3
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- Exercise 4.
last1 l = head $ reverse l

last2 l = head $ drop (length l - 1) l

-- Exercise 5.
init1 l = reverse $ drop 1 $ reverse l

init2 l = take (length l - 1) l
