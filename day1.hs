



-- Main function
main = do
  -- Parse the data
  contents <- getContents
  let in_data = lines contents
  putStrLn . show . length $ in_data
