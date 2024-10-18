isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

isOdd :: Int -> Bool
isOdd x = not (isEven x)

countEvens :: [Int] -> Int
countEvens = foldr (\x acc -> if isEven x then acc + 1 else acc) 0

countOdds :: [Int] -> Int
countOdds = foldr (\x acc -> if isOdd x then acc + 1 else acc) 0

concatString :: [String] -> String
concatString = foldl1 (\acc x -> acc ++ " " ++ x)  