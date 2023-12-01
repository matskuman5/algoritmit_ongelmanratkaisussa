import Data.List

-- getIncreasingSequence is a function that takes an integer n and a sequence of integers ns.
-- It returns an increasing subsequence of integers starting with n.
-- The function uses foldl' to iterate over the list, checking if each element is equal to the previous element plus 1.
-- If it is, the element is added to the result list, otherwise it is skipped.
getIncreasingSequence :: Int -> [Int] -> [Int]
getIncreasingSequence n ns = foldl' (\acc x -> if x == head acc + 1 then x : acc else acc) [n] (dropWhile (/= n) ns)

sequences :: Int -> [Int] -> [[Int]]
sequences _ [] = []
sequences n ns = incSeq : sequences (head incSeq + 1) (ns \\ incSeq)
    where incSeq = getIncreasingSequence n ns

main :: IO ()
main = do
    _ <- getLine
    input <- getLine
    let array = map read $ words input
    print (length (sequences 1 array))
    return ()