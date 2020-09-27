import Data.List (find)
import Data.Maybe (fromJust)

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

unfold :: (a -> Maybe (a, b)) -> a -> [b]
unfold f x = maybe [] (\(u, v) -> v : (unfold f u)) (f x)

binaryUnfolder :: Int -> Maybe (Int, Int)
binaryUnfolder 0 = Nothing
binaryUnfolder i = Just (div i 2, mod i 2)

binaryDigits :: Int -> [Int]
binaryDigits 0 = [0]
binaryDigits i = reverse (unfold binaryUnfolder i)

multipliersHelper :: Int -> [Int] -> [Int]
multipliersHelper i a@(x:xs) 
                        | i == 1       = []
                        | mod i x == 0 = x : multipliersHelper (div i x) a
                        | otherwise    = multipliersHelper i xs 

multipliers :: Int -> [Int]
multipliers 0 = []
multipliers i = multipliersHelper i primesEratosthenes

multipliersUnfolder :: Int -> Maybe (Int, Int)
multipliersUnfolder 1 = Nothing
multipliersUnfolder i = Just (div i x, x)
                        where x = fromJust (find (\x -> (mod i x) == 0) primesEratosthenes)