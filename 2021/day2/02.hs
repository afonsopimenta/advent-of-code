import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

getInput :: IO [(String, Int)]
getInput = map (second read . splitOnChar ' ') . lines <$> readFile "02-input.txt"
  where splitOnChar char str = splitAt (fromMaybe 0 $ elemIndex char str) str

part1 :: [(String, Int)] -> (Int, Int)
part1 = foldl calculateNextPos (0,0) where
  calculateNextPos (x, y) (dir, amount) = case dir of
    "forward" ->  (x + amount, y)
    "down" -> (x, y + amount)
    "up" -> (x, y - amount)
    _ -> (x, y)

part2 :: [(String, Int)] -> (Int, Int)
part2 = (\(a, b, c) -> (a, b)) . foldl calculateNextPos (0,0,0) where
  calculateNextPos (x, y, aim) (dir, amount) = case dir of
    "forward" ->  (x + amount, y + (amount * aim), aim)
    "down" -> (x, y, aim + amount)
    "up" ->(x, y, aim - amount)
    _ -> (x, y, aim)

main :: IO ()
main = do
  input <- getInput

  let ans1 = part1 input
  putStrLn $ unwords ["Part 1:", show ans1]
  
  let ans2 = part2 input
  putStrLn $ unwords ["Part 2:", show ans2]