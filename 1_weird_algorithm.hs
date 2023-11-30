intsToString :: [Int] -> String
intsToString = unwords . map show

collatzSequence :: Int -> [Int]
collatzSequence i
    | i /= 1 = i : collatzSequence (collatz i)
    | otherwise = [1]

collatz :: Int -> Int
collatz i
    | even i = i `div` 2
    | otherwise = 3 * i + 1

main :: IO ()
main = do
    i <- readLn
    putStrLn (intsToString (collatzSequence i))
    return ()