main = do
    content <- readFile "input"
    putStrLn $ show $ result (lines content) ['a'..'z']


result :: (Eq a) => [[a]] -> [a] -> Int
result input refList =
    let
        occuredList nr = map (\x -> any (\y -> isValueOccuringAmountOfTimes x y nr) refList) input
        occurences nr = count (occuredList nr) True
    in
        (occurences 2) * (occurences 3)

isValueOccuringAmountOfTimes :: (Eq a) => [a] -> a -> Int -> Bool
isValueOccuringAmountOfTimes list refVal amountOfTimes =
    count list refVal == amountOfTimes


count :: (Eq a) => [a] -> a -> Int
count list refVal =
    count' list refVal 0
    where
        count' :: (Eq a) => [a] -> a -> Int -> Int
        count' [] _ nrOccurences = nrOccurences
        count' (x:xs) refVal carry =
            if x == refVal
                then count' xs refVal (carry + 1)
                else count' xs refVal carry
