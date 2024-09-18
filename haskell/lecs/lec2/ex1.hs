-- test1 :: Int -> Int
-- test1 x = x + x

-- test2 :: Int -> Int -> Int
-- test2 x y = test1 x + test1 y

-- max1 :: (Ord a) => a -> a -> a
-- max1 x y
--     | x > y = x
--     | otherwise = y


myhead :: [Int] -> Int
myhead [] = error "empty list"
myhead (x:_) = x

mylast :: [Int] -> Int
mylast [] = error "empty list"
mylast [x] = x
mylast (_:xs) = mylast xs   -- mylast [1,2,3] = mylast [2,3] = mylast [3] = 3

delfirst :: [Int] -> [Int]
delfirst [] = error "empty list"
delfirst (_:xs) = xs -- delfirst [1,2,3] = [2,3]

dellast :: [Int] -> [Int]
dellast [] = error "empty list"
dellast [x] = []
dellast (x:xs) = x : dellast xs 
-- dellast [1,2,3] = 1 : dellast [2,3] = 1 : 2 : dellast [3] = 1 : 2 : []

swapfl :: [Int] -> [Int]
swapfl [] = error "empty list"
swapfl [x] = [x]
swapfl (x:xs) = mylast xs :  dellast xs ++ [x] 


getByIndex :: [Int] -> Int -> Int
getByIndex [] index = error "empty list"
getByIndex (x:xs) index 
    | index < 0 = error "index is negative"
    | index >= length (x:xs) = error "index is out of bounds"
    | index == 0 = x
    | otherwise = getByIndex xs (index - 1)

removeAllOccur ::  [Int] -> Int -> [Int]
removeAllOccur [] _ = []
removeAllOccur (x:xs) num
    | x == num = removeAllOccur xs num
    | otherwise = x : removeAllOccur xs num

getRepCount :: [Int] -> Int -> Int
getRepCount [] _ = 0
getRepCount (x:xs) num
    | x == num = 1 + getRepCount xs num 
    | otherwise = getRepCount xs num -- getRepCount [1,1,3,4,1] 1 -> 1 + getRepCount [1,3,4,1] 1 -> 1 + 1 + getRepCount [3,4,1] 1 -> 1 + 1 + getRepCount [4,1] 1 -> 1 + 1  + getRepCount[1] 1 -> 1 + 1 + 1 getRepCount [] 1 -> 1 + 1 + 1 + 0 -> 3
 
checkRep :: [Int] -> Int -> Bool
checkRep [] _ = False
checkRep (x:xs) num
    | getRepCount (x:xs) num > 1 = True
    | otherwise = False

-- удаляет все вхождения
removeRepNums ::  [Int] -> [Int]
removeRepNums [] = []
removeRepNums (x:xs)
    | checkRep (x:xs) x = removeRepNums (removeAllOccur xs x)
    | otherwise = x : removeRepNums xs

-- оставляет первое вхождение
removeRepNumsB ::  [Int] -> [Int]
removeRepNumsB [] = []
removeRepNumsB (x:xs)
    | checkRep (x:xs) x = x : removeRepNumsB (removeAllOccur xs x)
    | otherwise = x : removeRepNumsB xs

-- removeRepNums [1,1,2,3,4,2,1] -> removeRepNums ( removeAllOccur [1,1,2,3,4,2,1] 1 -> [2,3,4,2] ) -> removeRepNums [2,3,4,2] -> removeRepNums (removeAllOccur [2,3,4,2] 2 -> [3,4]) -> [3,4]

