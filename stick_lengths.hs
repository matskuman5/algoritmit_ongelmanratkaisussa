import Data.List

median :: [Int] -> Int
median ints
  | null ints = 0
  | odd len = ints !! (len `div` 2)
  | even len = ints !! (len `div` 2 - 1)
        where len = length ints

requiredMoves :: Int -> [Int] -> Int
requiredMoves i = foldl (\acc x -> acc + abs (x - i)) 0

main = do
  _ <- getLine
  input <- getLine
  let array = map read $ words input
  let med = median $ sort array
  print $ requiredMoves med array
  return ()
