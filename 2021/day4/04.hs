type Board = [[Int]]
type BoardData = [[(Int, Bool)]]

getInput :: IO ([Int],[Board])
getInput = do
  lines <- lines <$> readFile "04-input.txt"
  let numberSequence = map read $ splitOn ',' $ head lines
  let bingoBoards = map (map (map read . words)) $ splitOn "" (tail $ tail lines)
  return (numberSequence, bingoBoards)
  where
  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn splitter = foldr (\element acc -> if element == splitter then []:acc else (element : head acc) : tail acc) [[]]

part1 :: ([Int], [Board]) -> Int
part1 (sequence, boards) = part1' (sequence, map boardToBoardData boards) where
  part1' ([], _) = error "No board won!"
  part1' (currentNumber : nextNumbers, boardsData) = if null winnerBoards 
    then part1' (nextNumbers, updatedBoards)
    else calculateBoardScore currentNumber $ head winnerBoards
    where
      updatedBoards = map (map (map (\(num, marked) -> if num == currentNumber then (num, True) else (num, marked)))) boardsData
      winnerBoards = filter (\board -> hasCompleteRow board || hasCompleteColumn board) updatedBoards

part2 :: ([Int], [Board]) -> Int
part2 (sequence, boards) = part2' (sequence, map boardToBoardData boards) where
  part2' ([], _) = error "There is more than 1 loser board"
  part2' (currentNumber : nextNumbers, boardsData) = if length notWinnerBoards > 1
    then part2' (nextNumbers, notWinnerBoards)
    else let (lastMarkedNumber, lastBoardAfterWin) = finishBoard nextNumbers $ head notWinnerBoards in calculateBoardScore lastMarkedNumber lastBoardAfterWin
    where
      updatedBoards = map (updateBoard currentNumber) boardsData
      notWinnerBoards = filter (not . hasWon) updatedBoards

      finishBoard :: [Int] -> BoardData -> (Int, BoardData)
      finishBoard [] _ = error "Board didn't win"
      finishBoard (currentNumber : nextNumbers) boardData = if not (hasWon updatedBoard)
        then finishBoard nextNumbers updatedBoard
        else (currentNumber, updatedBoard)
        where updatedBoard = updateBoard currentNumber boardData

hasWon :: BoardData -> Bool
hasWon board = hasCompleteRow board || hasCompleteColumn board

hasCompleteRow :: BoardData -> Bool
hasCompleteRow = any (all snd)

hasCompleteColumn :: BoardData -> Bool
hasCompleteColumn board = any (\index -> all (snd . (!! index)) board) [0..(length (head board) - 1)]

updateBoard :: Int -> BoardData -> BoardData
updateBoard number = map (map (\(num, marked) -> if num == number then (num, True) else (num, marked)))

boardToBoardData :: Board -> BoardData
boardToBoardData = map (map (\x -> (x, False)))

calculateBoardScore :: Int -> BoardData -> Int
calculateBoardScore lastNumber boardData = lastNumber * sum notMarkedCells where
  numbersList = concat boardData
  notMarkedCells = map fst $ filter (\(_, marked) -> not marked) numbersList

main :: IO ()
main = do
  input <- getInput

  let ans1 = part1 input
  putStrLn $ unwords ["Part 1:", show ans1]
  
  let ans2 = part2 input
  putStrLn $ unwords ["Part 2:", show ans2]