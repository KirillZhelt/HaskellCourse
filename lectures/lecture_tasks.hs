factorial :: Int -> Integer
factorial n
    | n == 0    = 1
    | otherwise = (toInteger n) * (factorial $n - 1)

factorialHelper :: Int -> Int -> Integer
factorialHelper n a
    | n == 0    = toInteger a
    | otherwise = factorialHelper (n - 1) (n * a)

factorialTail :: Int -> Integer
factorialTail n = factorialHelper n 1

fib :: Int -> Integer
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib(n - 1) + fib(n - 2)

fibHelper :: Int -> Int -> Int -> Integer
fibHelper n a b
    | n == 0    = toInteger a
    | otherwise = fibHelper (n - 1) b (a + b)

fibTail :: Int -> Integer
fibTail n = fibHelper n 0 1

lengthX :: [a] -> Int
lengthX [] = 0
lengthX (_:xs) = 1 + lengthX xs

atXHelper :: [a] -> Int -> Int -> a
atXHelper (element:xs) n current
    | current == n  = element
    | otherwise = atXHelper xs n (current + 1)

atX :: [a] -> Int -> a
atX l n = atXHelper l n 0

reverseXHelper :: [a] -> [a] -> [a]
reverseXHelper [] ys = ys
reverseXHelper (x:xs) ys = reverseXHelper xs (x:ys)

reverseX :: [a] -> [a]
reverseX xs = reverseXHelper xs []

-- [1, 2, 3, 4] -> 
--    [[], [1], [2], [3], [4], [1, 2], [2, 3], [3, 4], [2, 4], [1, 4], [1, 2, 3], [2, 3, 4], [1, 3, 4], [1, 2, 4], [1, 2, 3, 4]]
booleanX :: [a] -> [[a]]
booleanX [] = [[]]
booleanX (x: xs) = let ys = booleanX(xs)
                    in ys ++ map (x:) ys

filterX :: (a -> Bool) -> [a] -> [a]
filterX _ [] = []
filterX f (x:xs)
    | f x == True = x : filterX f xs 
    | otherwise = filterX f xs

fibGen a b = a : (fibGen b (a + b))

fibEndless = fibGen 0 1

fibZipWith = 0 : 1 : (zipWith (+) fibZipWith (tail fibZipWith))

data Point = Point Int Int deriving (Show)
movePoint :: Point -> Int -> Int -> Point
movePoint (Point x y) u v = Point (x + u) (y + v)

data List a = Nil
            | Cons a (List a)

unfold :: (a -> Maybe (a, b)) -> a -> [b]
unfold f x = maybe [] (\(u, v) -> v : (unfold f u)) (f x)

down 0 = Nothing
down x = Just (x - 1, x)
