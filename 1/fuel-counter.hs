-- I want a function which does the calculation outlined in the puzzle definition:
-- "to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2."
fuel :: Int -> Int
fuel x = (x `quot` 3) - 2

-- For the Second part, we now need to take into account the weight of the fuel.
-- The function signature does not change, we still are given an integer and want an integer back
fuelPartTwo :: Int -> Int
fuelPartTwo x
    -- In the case that the fuel weight would be less than one, add nothing to the fuel
    | fuel x < 1 = 0    
    -- Otherwise, return the amount of fuel, plus the amount of fuel needed for the fuel!
    | otherwise = (fuel x) + fuelPartTwo (fuel x)

-- I will give the data as a text file, which one integer on each line.
-- It will by default read in as an a character array, but I want a list of integers,
-- so the type signature is:
parseStr :: [Char] -> [Int]
parseStr x =
    -- The character array should be split into lines wherever a newline appears.
    -- Haskell has a function built in for doing this, called 'lines'
    let xLines = lines x
    -- We then map this list to a list of integers, using the 'read' function.
    in map (read::String->Int) xLines

main = do
    input <- readFile "input.txt"
    let inputInts = parseStr input

    print (sum (map fuelPartTwo inputInts))
