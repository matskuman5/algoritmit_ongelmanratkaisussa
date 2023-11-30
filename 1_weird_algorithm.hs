intsToString :: [Int] -> String
intsToString = unwords . map show

collatzSequence :: Int -> [Int]
collatzSequence i = i : collatzSequence (collatz i)

collatz :: Int -> Int
collatz i
    | even i = i `div` 2
    | otherwise = 3 * i + 1

main :: IO ()
main = do
    i <- readLn
    putStrLn (intsToString (takeWhile (/= 1) (collatzSequence i) ++ [1]))
    return ()