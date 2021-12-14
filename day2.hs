import Control.Monad.State

-- Define custom data
data Direction = Forward | Up | Down deriving (Eq, Show)
type Instruction = (Direction, Int)
type Location = (Int, Int) -- (horizontal, depth)
type Location2 = (Int, Int, Int) -- (horizontal, depth, aim)

readInstruction :: String -> Instruction
readInstruction in_str = (dir, amt)
  where in_split = words in_str
        dir_str = head in_split
        amt = read . last $ in_split
        dir = case dir_str of
                        "forward" -> Forward
                        "up" -> Up
                        "down" -> Down

positionProd :: Location -> Int
positionProd (x, y) = x*y

positionProd2 :: Location2 -> Int
positionProd2 (x, y, _) = x*y

-- Methods for state navigation
navigateSub :: Instruction -> Location -> ((), Location)
navigateSub (dir, amt) (x, y) = case dir of
                                  Forward -> ((), (x + amt, y))
                                  Up -> ((), (x, max 0 (y - amt)))
                                  Down -> ((), (x, y + amt))

navigateSub2 :: Instruction -> Location2 -> Location2
navigateSub2 (dir, amt) (x, y, aim) = case dir of
                                        Forward -> (x + amt, max 0 (y + amt*aim), aim)
                                        Up -> (x, y, aim - amt)
                                        Down -> (x, y, aim + amt)

navigateSubM :: Instruction -> State Location ()
navigateSubM x = state $ navigateSub x

navigateSub2M :: Instruction -> State Location2 ()
navigateSub2M x = state $ \l -> ((), navigateSub2 x l)

getLocation :: State Location Int
getLocation = state $ \x -> (positionProd x, x)

getLocation2 :: State Location2 Int
getLocation2 = state $ \x -> (positionProd2 x, x)

-- An example of how to plan a trip
basicTrip :: State Location Int
basicTrip = do
  navigateSubM (Down, 5)
  navigateSubM (Down, 5)
  navigateSubM (Down, 5)
  navigateSubM (Down, 5)
  navigateSubM (Down, 5)
  navigateSubM (Down, 5)
  getLocation

executeTrip :: [Instruction] -> State Location Int
executeTrip instrs = do
  forM_ instrs navigateSubM
  getLocation

executeTrip2 :: [Instruction] -> State Location2 Int
executeTrip2 instrs = do
  forM_ instrs navigateSub2M
  getLocation2

-- Main function
main = do
  -- Parse data
  contents <- getContents
  let in_data = (map readInstruction) . lines $ contents
      part1 = runState (executeTrip in_data) (0, 0)
      part2 = runState (executeTrip2 in_data) (0, 0, 0)

  putStrLn $ "Part 1: " ++ (show . fst $ part1)
  putStrLn $ "Part 2: " ++ (show . fst $ part2)
