main = do
    content <- readFile "input"
    putStrLn $ show $ accFreqs $ content


accFreqs :: String -> Int
accFreqs input =
    f (lines input) 0
    where
        f :: [String] -> Int -> Int
        f [] freq = freq
        f (x:xs) freq =
            f xs (nextFreq x freq)



nextFreq :: String -> Int -> Int
nextFreq (operatorChar:diffString) currFreq =
    let operator = if operatorChar == '+'
        then (+)
        else (-)
        diff = read diffString
    in
        currFreq `operator` diff


