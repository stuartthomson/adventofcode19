import Debug.Trace

-- Returns a new list based on the input, with one of the values replaced
replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt index value l = head ++ [value] ++ (drop 1 rest)
    where (head, rest) = splitAt index l

-- This function returns an updated version of the program
-- It takes the program as the first argument, and then the index
-- of the specific operation location
performOp :: [Int] -> Int -> [Int] 
performOp program i = replaceAt dest (op a b) program
    where dest = program !! (i+3)
          a = program !! (program !! (i+1))
          b = program !! (program !! (i+2))
          op x y = case (program !! i) of
            1 -> x + y
            2 -> x * y

process :: (Int, [Int]) -> (Int, [Int])
-- process (index, program) | trace ("process " ++ show index ++ " " ++ show (program !! 0)) False = undefined
process (index, program)
    | program !! index == 99 = (index, program)
    | otherwise = process (index + 4, updated_program)
    where updated_program = performOp program index

testProgram = [1,9,10,3,2,3,11,0,99,30,40,50]
program1 = [1,0,0,0,99]
program2 = [2,3,0,3,99]
program3 = [2,4,4,5,99,0]
program4 = [1,1,1,4,99,5,6,0,99]

splitCommas :: [Char] -> [String]
splitCommas x
    | notElem ',' x  = [x]
    | otherwise = first : rest
    where first = takeWhile (/=',') x
          rest = splitCommas (drop 1 (dropWhile (/=',') x)) -- drop 1 to remove comma from head

parseStr :: [Char] -> [Int]
parseStr x =
    let values = splitCommas x
    in map (read::String->Int) values

nextPair :: (Int, Int) -> (Int, Int)
nextPair (noun, verb)
    | verb == 99 = ((noun+1), 0)
    | otherwise = (noun, (verb+1))

tryValues :: [Int] -> (Int, Int) -> (Int, Int)
tryValues program (noun, verb) | trace ("tryValues " ++ show noun ++ " " ++ show verb) False = undefined
tryValues program (noun, verb)
    | ((noun > 99) || (verb > 99)) = (999, 999)
    | (snd completeProgram) !! 0 == 19690720 = (noun, verb)
    | otherwise = tryValues program (nextNoun, nextVerb)
    where newProgram = replaceAt 2 verb $ replaceAt 1 noun program
          completeProgram = process (0, newProgram)
          (nextNoun, nextVerb) = nextPair (noun, verb)

main = do 
    putStrLn "----Test Programs----"
    print $ process (0, testProgram)
    print $ process (0, program1)
    print $ process (0, program2)
    print $ process (0, program3)
    print $ process (0, program4)
    putStrLn "----Parsing Input----"
    input <- readFile "input.txt"
    print input
    let initialProgram = parseStr input
    let actualProgram = replaceAt 2 2 $ replaceAt 1 12 initialProgram
    putStrLn "--------Result-------"
    print $ process (0, actualProgram)
    print $ tryValues initialProgram (0, 0)
    