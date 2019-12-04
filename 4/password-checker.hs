cycleByOne :: [Int] -> [Int]
cycleByOne password = take (length password) $ drop 1 $ cycle password

-- The 'or' function is a little counterintuive here. It returns True if any
-- element in the array is True
pair :: [Int] -> Bool
pair password = or $ zipWith (==) (init password) (init (cycleByOne password))

-- This is so similar to the 'pair' function that probably I should do something cleverer.
decreases :: [Int] -> Bool
decreases password = or $ zipWith (>) (init password) (init (cycleByOne password))

getDigits :: Int -> [Int]
getDigits x
    | x == 0 = []
    | otherwise = getDigits (x `div` 10) ++ [x `mod` 10]

valid :: Int -> Bool
valid value = 
    (pair password) && not (decreases password)
    where password = getDigits value

toInt :: Bool -> Int
toInt val
    | val == True = 1
    | otherwise = 0

countValidPasswords :: Int -> Int -> Int
countValidPasswords min max =
    sum $ map toInt bools 
    where bools = [ valid val | val <- [min..max] ]

main = do
    print $ countValidPasswords 240920 789857