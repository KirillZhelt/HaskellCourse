primes :: [Int]
primes = sieve [2..]
            where 
            sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

minus :: Ord a => [a] -> [a] -> [a]
minus a@(x:xs) b@(y:ys) = case compare x y of
         LT -> x : minus  xs b
         EQ ->     minus  xs ys
         GT ->     minus  a  ys
minus xs ys = xs

primesEratosthenes :: [Int]
primesEratosthenes = sieve [2..] 
           where
           sieve (p:xs) = p : sieve (minus xs [p, p + p..])

foldlX :: (a -> b -> a) -> a -> [b] -> a
foldlX _ a [] = a
foldlX f a (x:xs) = foldlX f (f a x) xs

foldrX :: (a -> b -> b) -> b -> [a] -> b
foldrX _ b [] = b
foldrX f b (x:[]) = f x b
foldrX f b (x:xs) = f x (foldrX f b xs)