cycleByOne :: [a] -> [a]
cycleByOne password = take (length password) $ drop 1 $ cycle password

compareWithNext :: (a -> a -> b) -> [a] -> [b]
compareWithNext comparator l = zipWith comparator (init l) (init (cycleByOne l))

pair :: [Int] -> Bool
pair password = or $ compareWithNext (==) password

decreases :: [Int] -> Bool
decreases password = or $ compareWithNext (>) password

getDigits :: Int -> [Int]
getDigits x
    | x == 0 = []
    | otherwise = getDigits (x `div` 10) ++ [x `mod` 10]

valid :: Int -> Bool
valid value = 
    (pair password) && not (decreases password) && (theresABlockOfTwo password)
    where password = getDigits value

toInt :: Bool -> Int
toInt val
    | val == True = 1
    | otherwise = 0

countValidPasswords :: Int -> Int -> Int
countValidPasswords min max =
    sum $ map toInt bools 
    where bools = [ valid val | val <- [min..max] ]

blocks :: [Int] -> [Int]
blocks l
    | l == [] = [0]
    | otherwise = [length (takeWhile (== head l) l)] ++ (blocks $ dropWhile (== head l) l)

theresABlockOfTwo :: [Int] -> Bool
theresABlockOfTwo l = or $ map (2==) $ blocks l

main = do
    print $ countValidPasswords 240920 789857