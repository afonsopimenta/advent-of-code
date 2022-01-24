import Data.Char (digitToInt)
import Data.List (foldl')

getInput :: IO [String]
getInput = lines <$> readFile "03-input.txt"

part1 :: [String] -> Int
part1 ls = binaryToDecimal gammaRate * binaryToDecimal epsilonRate where
  gammaRate = calculateGammaRate ls
  epsilonRate = map flipBit gammaRate

  calculateGammaRate :: [String] -> String
  calculateGammaRate = calculateGammaRate' 0 where
    calculateGammaRate' index ls = if index < length (head ls)
      then getMostCommonBit (map (!! index) ls) : calculateGammaRate' (index + 1) ls
      else []

part2 :: [String] -> Int
part2 ls = binaryToDecimal oxygenGeneratorRating * binaryToDecimal co2ScrubberRating where
  oxygenGeneratorRating = calculateOxygenGeneratorRating ls
  co2ScrubberRating = calculateCO2ScrubberRating ls

  calculateOxygenGeneratorRating :: [String] -> String
  calculateOxygenGeneratorRating = calculateOxygenGeneratorRating' 0 where
    calculateOxygenGeneratorRating' index ls = if length ls > 1
      then calculateOxygenGeneratorRating' (index + 1) $ filter (\byte -> byte !! index == mostCommomBitAtIndex) ls
      else head ls
      where mostCommomBitAtIndex = getMostCommonBit (map (!! index) ls)

  calculateCO2ScrubberRating :: [String] -> String
  calculateCO2ScrubberRating = calculateCO2ScrubberRating' 0 where
    calculateCO2ScrubberRating' index ls = if length ls > 1
      then calculateCO2ScrubberRating' (index + 1) $ filter (\byte -> byte !! index == leastCommomBitAtIndex) ls
      else head ls
      where leastCommomBitAtIndex = flipBit $ getMostCommonBit (map (!! index) ls)

getMostCommonBit :: String -> Char
getMostCommonBit ls = if zerosAmount > onesAmount then '0' else '1'
  where (zerosAmount, onesAmount) = foldr (\bit (zeros, ones) -> if bit == '0' then (1 + zeros, ones) else (zeros, 1 + ones)) (0,0) ls

flipBit :: Char -> Char
flipBit bit = if bit == '0' then '1' else '0'

binaryToDecimal :: String -> Int
binaryToDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

main :: IO ()
main = do
  input <- getInput

  let ans1 = part1 input
  putStrLn $ unwords ["Part 1:", show ans1]
  
  let ans2 = part2 input
  putStrLn $ unwords ["Part 2:", show ans2]