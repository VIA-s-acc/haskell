import Data.Ratio
import Data.Text (replace)
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



getPivotStep :: (Num a, Eq a) => a -> [a] -> a
getPivotStep num [] = 0
getPivotStep num (x:xs)
    | num == 1 = x
    | otherwise = getPivotStep (num-1) xs

subtractRow :: Num t => t -> [t] -> [t] -> [t]
subtractRow _ [] [] = []
subtractRow coeff (x:xs) (y:ys) = (x - coeff * y) : subtractRow coeff xs ys

applyGaussStep :: (Eq a, Num a, Fractional a) => a -> a -> [a] -> [[a]] -> [[a]]
applyGaussStep pivot zcounter [] rows = rows
applyGaussStep pivot zcounter pivotRow rows = map ( \row -> subtractRow ( getPivotStep zcounter row / pivot ) row pivotRow) rows 

swapRows :: (Eq a, Num a) => [[a]] -> [[a]]
swapRows (row:rows)
    | head row /= 0 = row : rows  
    | otherwise =
        let nonZeroRows = filter (\r -> head r /= 0) rows  
        in if null nonZeroRows 
            then row : rows  
            else let (firstNonZero:_) = nonZeroRows 
                    in firstNonZero : replaceRow rows firstNonZero row
                    where 
                        replaceRow [] _ _ = []
                        replaceRow (r:rs) firstNonZero row2swap
                            | r == firstNonZero = row2swap : rs
                            | otherwise = r : replaceRow rs firstNonZero row2swap

getPivot :: (Num a, Eq a) => [a] -> a
getPivot [] = 0
getPivot (x:xs) 
    | x /= 0 = x
    | otherwise = getPivot xs

getLen :: (Num a) => [a] -> a
getLen = foldr (\ x -> (+) 1) 0

gaussElimination_ :: (Ord a, Fractional a) => a -> [[a]] -> [[a]]
gaussElimination_ _ [] = []
gaussElimination_ zcounter matrix =
    let swappedMatrix = replaceSmallNumbers 1e-15 (swapRows matrix) 
        row = head swappedMatrix
        rows = tail swappedMatrix
        pivot = getPivot row
        in if pivot == 0 || zcounter == getLen row then
                swappedMatrix
            else
                row
                : gaussElimination_
                    (zcounter + 1) (applyGaussStep pivot zcounter row rows)

gaussElimination :: (Ord b, Fractional b) => [[b]] -> [[b]]
gaussElimination [] = error "empty matrix"
gaussElimination matrix = gaussElimination_ 1 matrix

isZeroRow :: (Eq a, Num a) => [a] -> Bool
isZeroRow = all (== 0)

replaceSmallNumbers :: (Ord b, Num b) => b -> [[b]] -> [[b]]
replaceSmallNumbers epsilon = map (map (\ x -> if abs x < epsilon then 0 else x))

getRank :: (Ord a, Fractional a) => [[a]] -> Int
getRank [] = 0
getRank matrix = 
    let gaussMatrix = gaussElimination matrix
    in length (filter ( not . isZeroRow ) gaussMatrix)

main :: IO ()
main = do
    -- let matrix = [[1, 1, -1, -1],   
    --               [0, -1, 2, -1],
    --               [0, 0, 2, -1]]
    let matrix = [[3,2,-5,7],
                  [2,-1,3,6],
                  [1,2,-1,5],
                  [1,1,-1,4]]
    let result = gaussElimination_ 1 matrix
    mapM_ print result
    -- let rank = getRank matrix
    -- print rank