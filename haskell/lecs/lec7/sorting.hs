import System.Random (randomRIO)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Monad (replicateM, forM_)  -- Add this import
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

randomArray :: Int -> Double -> Double -> IO [Double]
randomArray n minVal maxVal = mapM (\_ -> randomRIO (minVal, maxVal)) [1..n]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (uncurry (<=)) (zip xs (tail xs))

medianOfThree :: Ord a => [a] -> a
medianOfThree xs = 
    let a = head xs
        b = last xs
        c = xs !! (length xs `div` 2)
    in median [a, b, c]
  where
    median [x, y, z] 
        | x <= y && y <= z = y
        | x <= z && z <= y = z
        | z <= x && x <= y = x
        | z <= y && y <= x = y
        | y <= x && x <= z = x
        | y <= z && z <= x = z


dualPivotQuickSort :: Ord a => [a] -> [a]
dualPivotQuickSort [] = []
dualPivotQuickSort [x] = [x]
dualPivotQuickSort (pivot1:pivot2:xs) | pivot1 > pivot2 = dualPivotQuickSort (pivot2:pivot1:xs)
dualPivotQuickSort (pivot1:pivot2:xs) = 
    let (low, high, middle) = partition3 pivot1 pivot2 xs
    in dualPivotQuickSort low ++ [pivot1] ++ dualPivotQuickSort middle ++ [pivot2] ++ dualPivotQuickSort high

lastSafe :: [a] -> a -> a
lastSafe [] y = y 
lastSafe xs _ = last xs 

dualPivotQuickSortMedian :: Ord a => [a] -> [a]
dualPivotQuickSortMedian [] = []
dualPivotQuickSortMedian [x] = [x]
dualPivotQuickSortMedian (x:y:xs) = 
    let pivot1 = medianOfThree [x, y, lastSafe xs y]  
        pivot2 = lastSafe xs y
    in if pivot1 > pivot2
        then dualPivotQuickSortMedian (pivot2:pivot1:xs)
        else let (low, high, middle) = partition3 pivot1 pivot2 xs
             in dualPivotQuickSortMedian low ++ [pivot1] ++ dualPivotQuickSortMedian middle ++ [pivot2] ++ dualPivotQuickSortMedian high

-- Функция для разбиения массива на три части относительно двух опорных элементов
partition3 :: Ord a => a -> a -> [a] -> ([a], [a], [a])
partition3 pivot1 pivot2 xs = (low, high, middle)
  where
    low = [x | x <- xs, x < pivot1 ]
    high = [x | x <- xs, x > pivot2]
    middle = [x | x <- xs, x >= pivot1 && x <= pivot2]


measureTime :: IO a -> IO (a, Integer)
measureTime action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = end - start
    return (result, diff)

testSorting :: Int -> IO ()
testSorting n = do
    let sizes = [100, 1000, 10000, 100000, 1000000, 10000000]
    let iterations = 100  -- Number of iterations for each test

    forM_ sizes $ \size -> do
        putStrLn $ "Testing array size " ++ show size ++ " for " ++ show iterations ++ " iterations."

        -- Variables to accumulate total times for each method
        let testLoop method = do
                -- Measure time for array generation
                let arrayGen = randomArray size 0 100
                (arr, timeGen) <- measureTime arrayGen

                -- Measure time for sorting
                (sortedArr, timeSort) <- measureTime (return $ method arr)
                return (timeGen, timeSort)

        -- Run the tests for each algorithm and calculate the average time
        dualPivotResults <- replicateM iterations (testLoop dualPivotQuickSort)
        quickSortResults <- replicateM iterations (testLoop quickSort)
        dualMedianResults <- replicateM iterations (testLoop dualPivotQuickSortMedian)

        -- Extracting the times for array generation and sorting
        let (dualPivotGenTimes, dualPivotSortTimes) = unzip dualPivotResults
        let (quickSortGenTimes, quickSortSortTimes) = unzip quickSortResults
        let (dualMedianGenTimes, dualMedianSortTimes) = unzip dualMedianResults

        -- Calculating average times
        let avgTime list = fromIntegral (sum list) / fromIntegral (length list) / 10^12 :: Double
        let avgGenTime list = avgTime list
        let avgSortTime list = avgTime list

        putStrLn $ "Average time for array generation: " ++ show (avgGenTime dualPivotGenTimes) ++ " sec"
        putStrLn $ "Average time for DualPivotQuickSort: " ++ show (avgSortTime dualPivotSortTimes) ++ " sec"
        putStrLn $ "Average time for QuickSort: " ++ show (avgSortTime quickSortSortTimes) ++ " sec"
        putStrLn $ "Average time for DualPivotQuickSort (Median): " ++ show (avgSortTime dualMedianSortTimes) ++ " sec"

main :: IO ()
main = do
    putStrLn "Вы хотите включить автотест? (True/False)"
    testIter <- readLn :: IO Bool
    
    if testIter then do
        putStrLn "Введите количество тестов:"
        testCount <- readLn
        testSorting testCount

    else do
        putStrLn "автотест отключён. Ручной режим."
        putStrLn "Введите длину массива:"
        n <- readLn
        putStrLn "Введите минимальное значение:"
        minVal <- readLn
        putStrLn "Введите максимальное значение:"
        maxVal <- readLn
        putStrLn "Вы хотите выводить массивы в консоль? (True/False)"
        printLists <- readLn :: IO Bool
        (arr, timeGen) <-  measureTime ( randomArray n minVal maxVal)
        putStrLn "Время генерации массива:"
        printf "Время выполнения: %.12f сек.\n" (fromIntegral timeGen / 10^12 :: Double)
        
        

        
        -- Измеряем время для DualPivotQuickSort
        (sortedArrDual, timeDual) <- measureTime (return $ dualPivotQuickSort arr)
        putStrLn "DualPivotQuickSort:"
        printf "Время выполнения: %.12f сек.\n" (fromIntegral timeDual / 10^12 :: Double)
        
        -- Измеряем время для QuickSort
        (sortedArr, timeQuick) <- measureTime (return $ quickSort arr)
        putStrLn "QuickSort:"
        printf "Время выполнения: %.12f сек.\n" (fromIntegral timeQuick / 10^12 :: Double)
        
        -- Измеряем время для DualPivotQuickSort с медианой
        (sortedArrDualMedian, timeDualMedian) <- measureTime (return $ dualPivotQuickSortMedian arr)
        putStrLn "DualPivotQuickSort с медианой:"
        printf "Время выполнения: %.12f сек.\n" (fromIntegral timeDualMedian / 10^12 :: Double)

        --Проверка сортировки
        -- putStrLn "Dual-flag:"
        -- print $ isSorted sortedArrDual
        -- putStrLn "Default-flag:"
        -- print $ isSorted sortedArr
        -- putStrLn "Dual-Median-flag:"
        -- print $ isSorted sortedArrDualMedian

        if printLists then do
            putStrLn "Случайный массив:"
            print arr
            putStrLn "Отсортированный массив (dual):"
            print sortedArrDual
            putStrLn "Отсортированный массив (quick):"
            print sortedArr
            putStrLn "Отсортированный массив (dual-median):"
            print sortedArrDualMedian
        else do
            putStrLn "Вывод массивов отключен."

-- main :: IO ()
-- main = do
--     testSorting 100  -- You can modify the parameter here for more test cases
