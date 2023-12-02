import Data.List

getIncreasingSequence :: Int -> [Int] -> [Int]
getIncreasingSequence n ns = foldl' (\acc x -> if x == head acc + 1 then x : acc else acc) [n] (dropWhile (/= n) ns)

sequences :: Int -> [Int] -> [[Int]]
sequences _ [] = []
sequences n ns 
    | n > maximum ns = []
    | otherwise = incSeq : sequences (head incSeq + 1) ns
            where incSeq = getIncreasingSequence n ns

main :: IO ()
main = do
    _ <- getLine
    input <- getLine
    let array = map read $ words input
    print (length (sequences 1 array))
    return ()