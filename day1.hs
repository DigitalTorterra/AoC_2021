
slidingWindow :: (Num a) => [a] -> [a]
slidingWindow xs = map (\(a, b, c) -> a + b + c) (zip3 xs (tail xs) (tail . tail $ xs))

countIncreases :: (Ord a) => [a] -> Int
countIncreases xs = length . (filter (\(a, b) -> a < b)) $ zip xs (tail xs)


-- Main function
main = do
  -- Parse the data
  contents <- getContents
  let in_data = map read (lines contents) :: [Integer]
      part_1 = countIncreases in_data
      part_2 = countIncreases . slidingWindow $ in_data

  putStrLn $ "Part 1: " ++ (show part_1)
  putStrLn $ "Part 2: " ++ (show part_2)
