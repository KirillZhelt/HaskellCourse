import Data.Map (fromListWith, toList)
import System.Environment (getArgs)

findFirstIndexOfAnyHelper :: [Char] -> String -> Int -> Maybe Int
findFirstIndexOfAnyHelper _ [] _ = Nothing
findFirstIndexOfAnyHelper chars (x:xs) currentIndex = if elem x chars
                                                        then Just currentIndex
                                                        else findFirstIndexOfAnyHelper chars xs $currentIndex + 1

findFirstIndexOfAny :: [Char] -> String -> Maybe Int
findFirstIndexOfAny chars a = findFirstIndexOfAnyHelper chars a 0

splitBy :: [Char] -> String -> [String]
splitBy separators s = filter (not . null) (splitByHelper separators s)
                        where splitByHelper separators s = case separatorIndex of
                                                            Just a -> (take a s) : splitBy separators (drop (a + 1) s)
                                                            Nothing -> [s]
                                                            where separatorIndex = (findFirstIndexOfAny separators s)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

countWords :: String -> [(String, Int)]
countWords text = frequency (splitBy [' ', '\n'] text)

main = do
    args <- getArgs
    let inputFile = head args
    fileContent <- readFile inputFile
    print (countWords fileContent)
