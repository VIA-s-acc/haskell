import Data.Ratio
-- просто вхождение (порядок не важен)
isSubListNO :: [Int] -> [Int] -> Bool
isSubListNO [] ys = True
isSubListNO xs [] = False
isSubListNO (x:xs) (y:ys)
    | x == y = isSubListNO xs ys
    | otherwise = isSubListNO (x:xs) ys

    -- isSubListNO [1,2] [1,2,3,4] -> isSubListNO [2] [2,3,4] -> isSubListNO [] [3,4] -> isSubListNO [] _ -> True
    -- isSubListNO [1,2,3] [1,2,4] -> isSubListNO [2,3] [2,4] -> isSubListNO [3] [4] -> isSubListNO [3] [] -> False
    -- isSubListNO [1,2,3] [1,2,4,3,5] -> isSubListNO [2,3] [2,4,3,5] -> isSubListNO [3] [4,3,5] -> isSubListNO [3] [3,5] -> isSubListNO [] [5] -> isSubListNO [] _ -> True

delfirst :: [a] -> [a]
delfirst [] = error "empty list"
delfirst (_:xs) = xs 

bringFirstN :: [Int] -> Int -> [Int]
bringFirstN [] _ = []
bringFirstN _ 0 = []
bringFirstN (x:xs) n = x : bringFirstN xs (n-1)

isSubList :: [Int] -> [Int] -> Bool
isSubList xs ys
    | length xs > length ys = False
    | bringFirstN ys (length xs) == xs = True
    | otherwise = isSubList xs (delfirst ys)


-- removeCol :: [Double] -> Int -> Int -> [Double]
removeCol :: (Eq t, Num t) => [a] -> t -> t -> [a]
removeCol [] _ _ = []
removeCol (x:xs) j currCol
        | j == currCol = removeCol xs j (currCol + 1)
        | otherwise = x : removeCol xs j (currCol + 1)

-- getEl :: [[Double]] -> Int -> Int -> Double
getEl :: [[a]] -> Int -> Int -> a
getEl xs i j 
    | i < 0 || j < 0 || i >= length xs || j >= length (xs !! i) = error "index out of bounds"
    | otherwise = xs !! i !! j

-- removeRowCol :: Int -> Int -> [[Double]] -> Int -> [[Double]]
removeRowCol :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> [[a]] -> t1 -> [[a]]
removeRowCol _ _ [] _ = []
removeRowCol i j (x:xs) currRow 
    | i == currRow = removeRowCol i j xs (currRow + 1)
    | otherwise = removeCol x j 0 : removeRowCol i j xs (currRow + 1)

-- getMinor :: Int -> Int -> [[Double]] -> [[Double]]
getMinor :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> [[a]] -> [[a]]
getMinor i j mat = removeRowCol i j mat 0

-- isSquare :: [[Double]] -> Bool
isSquare :: (Foldable t1, Foldable t2) => t1 (t2 a) -> Bool
isSquare mat = all (\row -> length row == length mat) mat

-- determinant :: [[Double]] -> Double
determinant :: Num a => [[a]] -> a
determinant [[x]] = x
determinant matrix 
    | isSquare matrix = sum [(-1) ^ col * getEl matrix 0 col  * determinant ( getMinor 0 col matrix) | col <- [0..(length matrix - 1)]]
    | otherwise = error "matrix is not square"
    
-- transpose :: [[Double]] -> [[Double]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose matrix = [ [ matrix !! i !! j | i <- [0..(length matrix - 1)] ] | j <- [0..(length (matrix !! 0) - 1)] ]

-- inverse :: [[Double]] -> [[Double]]
inverse :: (Eq a, Fractional a) => [[a]] -> [[a]]
inverse matrix
    | det == 0 = error "matrix is not invertible"
    | otherwise = [ [ ( (-1) ^ (i + j + 2) ) * determinant (getMinor j i matrix) / det | j <- [0..(length matrix - 1)] ] | i <- [0..(length matrix - 1)] ]
    where det = determinant matrix
        


