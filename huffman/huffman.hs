import Data.List as L
import Data.Map as M

data HuffmanTree v = Node Int (HuffmanTree v) (HuffmanTree v)
                   | Leaf v Int

getFrequency :: HuffmanTree a -> Int
getFrequency (Node f _ _) = f
getFrequency (Leaf _ f) = f

compareTrees :: HuffmanTree a -> HuffmanTree a -> Ordering
compareTrees t1 t2 
 | getFrequency t1 < getFrequency t2 = LT
 | getFrequency t1 > getFrequency t2 = GT
 | otherwise = EQ

toString :: Show a => HuffmanTree a -> String
toString (Leaf v p) = show v ++ "--" ++ show p
toString (Node p l r) = "< " ++ toString l ++ " | " ++ show p ++ " | " ++ toString r ++ " >" 

instance Show a => Show (HuffmanTree a) where
    show = toString

countListFrequency :: Ord a => [a] -> [(a, Int)]
countListFrequency list = M.toList $ M.fromListWith (+) [(c, 1) | c <- list]

toLeaf :: (a, Int) -> HuffmanTree a
toLeaf (v, p) = Leaf v p

toLeafs :: Ord a => [a] -> [HuffmanTree a]
toLeafs xs = L.map toLeaf (countListFrequency xs)

sortHuffmanTrees :: Ord a => [HuffmanTree a] -> [HuffmanTree a]
sortHuffmanTrees xs = sortBy compareTrees xs
            
toHuffmanTree :: Ord a => [a] -> HuffmanTree a
toHuffmanTree str = toHuffmanTreeHelper $ sortHuffmanTrees $ toLeafs str
toHuffmanTreeHelper :: Ord a => [HuffmanTree a] -> HuffmanTree a
toHuffmanTreeHelper (t:[]) = t
toHuffmanTreeHelper (t1:t2:xs) = case compareTrees t1 t2 of
    GT -> toHuffmanTreeHelper (sortHuffmanTrees (((Node (getFrequency t1 + getFrequency t2) t2 t1)):xs))
    otherwise -> toHuffmanTreeHelper (sortHuffmanTrees (((Node (getFrequency t1 + getFrequency t2) t1 t2)):xs))
    
fromHuffmanTreeToMap :: Ord a => HuffmanTree a -> M.Map a [String]
fromHuffmanTreeToMapHelper xs (Leaf v p) = [(v, (reverse xs))]
fromHuffmanTreeToMapHelper xs (Node p l r) = (fromHuffmanTreeToMapHelper ("0":xs) l) ++ (fromHuffmanTreeToMapHelper ("1":xs) r)
fromHuffmanTreeToMap t = M.fromListWith (++) (fromHuffmanTreeToMapHelper [] t)

encode :: String -> String
encodeHelper [] _ rez = rez
encodeHelper (s:str) map rez = encodeHelper str map ((findWithDefault [] s map) ++ rez)
encode s = concat $ encodeHelper (reverse s) (fromHuffmanTreeToMap $ toHuffmanTree s) []

invert :: M.Map a [String] -> M.Map String a
invert m = M.fromList $ L.map (\(k, vs) -> (vs, k)) $ L.map (\(d, a) -> (d,(concat a))) (M.toList m )

decode_helper rez tmp [] map = reverse rez
decode_helper rez tmp (s:str) map = case M.lookup (tmp ++ [s]) map of 
    Just v -> decode_helper (v:rez) [] (str) map
    Nothing -> decode_helper rez (tmp ++ [s]) str map
decode str map = decode_helper [] [] str (invert map)
