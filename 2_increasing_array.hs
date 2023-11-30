movesToEqual :: Int -> Int -> Int
movesToEqual n m
    | n <= m = 0
    | otherwise = n - m

sumOfMoves :: [Int] -> Int
sumOfMoves [] = 0
sumOfMoves [_] = 0
sumOfMoves (x:y:xs) = movesToEqual x y + sumOfMoves ((y + movesToEqual x y):xs)

main :: IO ()
main = do
    _ <- getLine
    input <- getLine
    let array = map read $ words input
    print (sumOfMoves array)
    return ()