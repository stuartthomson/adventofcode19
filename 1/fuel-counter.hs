-- I want a function which does the calculation outlined in the puzzle definition:
-- "to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2."
fuel :: Int -> Int
fuel x = (x `quot` 3) - 2

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

    print (sum (map fuel inputInts))
