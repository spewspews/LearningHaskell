-- Ex. 1
-- [Char]
-- (Char, Char, Char)
-- [(Bool, Char)]
-- ([Bool], [Char])
-- [[a] -> [a]]

-- Ex. 2
bools = [True, False]

nums :: [[Int]] = [[0, 1, 2], [3, 4, 5]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy a = (a, a)

apply f a = f a

-- Exs. 3 and 4
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair a b = (a, b)

double :: (Num a) => a -> a
double x = x * 2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Ex. 5. For functions whose either domain or codomain is not an instance of Eq, there is not a way to determine whether the arguments are equal or the results are equal so we cannot determine if the functions are equal. If two functions' domain is infinite then the comparison operator would need to evaluate the function at all arguments which cannot be done in finite time. Functions' whose domain is finite and a member of Eq and whose codomain is a member of Eq can be instances of the Eq class by evaluating the function at the finite number of members of the domain and checking if equal arguments result in equal results.
