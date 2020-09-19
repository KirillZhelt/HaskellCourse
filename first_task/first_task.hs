concatX :: [a] -> [a] -> [a]
concatX [] ys = ys 
concatX (x:xs) ys = x : concatX xs ys
 
concatXx :: [[a]] -> [a]
concatXx [] = []
concatXx ([]:ys) = concatXx ys
concatXx ((x:xs):ys) = x : concatXx (xs:ys) 

mapX :: (a -> b) -> [a] -> [b]
mapX f [] = []
mapX f (x:xs) = f x : mapX f xs

fmapX :: (a -> [b]) -> [a] -> [b]
fmapX f xs = concatXx (mapX f xs)

sinusHelper :: Int -> Int -> Double -> Double -> Double -> Double
sinusHelper n stepn x stepx current 
    | n + 1 == stepn = current
    | otherwise      = sinusHelper n (stepn + 1) x ((stepx * (-1.0) * x * x) / (fromIntegral ((2 * (stepn + 1)) * (2 * (stepn + 1) + 1)) :: Double)) (stepx + current)

sinusX :: Double -> Int -> Double
sinusX x n = sinusHelper n 1 x ((x * x  * x) / (-6.0)) x