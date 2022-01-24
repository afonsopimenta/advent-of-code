getInput :: IO [Int]
getInput = map read . lines <$> readFile "01-input.txt"

part1 :: [Int] -> Int
part1 ls = if length ls < 2
  then 0
  else foldr (\(x, y) acc -> if x < y then acc + 1 else acc) 0 (zip ls (tail ls))

part2 :: [Int] -> Int
part2 ls = if length ls < 4
  then 0
  else foldr (\(x, y) acc -> if x < y then acc + 1 else acc) 0 (zip sumOfDepthsList (tail sumOfDepthsList))
  where sumOfDepthsList = map (\(a, b, c) -> a + b + c) (zip3 ls (tail ls) (tail $ tail ls))

main :: IO ()
main = do
  input <- getInput

  let ans1 = part1 input
  putStrLn $ unwords ["Part 1:", show ans1]
  
  let ans2 = part2 input
  putStrLn $ unwords ["Part 2:", show ans2]