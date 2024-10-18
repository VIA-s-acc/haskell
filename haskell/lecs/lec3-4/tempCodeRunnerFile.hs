gaussElimination_ :: (Fractional a, Eq a) => a -> [[a]] -> [[a]]
gaussElimination_ _ [] = []
gaussElimination_ zcounter matrix =
    let swappedMatrix = swapRows matrix 
        row = head swappedMatrix
        in if zcounter == (length row) -1 then swappedMatrix
        rows = tail swappedMatrix
        pivot = getPivot row
        in if pivot == 0 
            then swappedMatrix 
            else row : gaussElimination_ (zcounter+1) (applyGaussStep pivot zcounter row rows) 