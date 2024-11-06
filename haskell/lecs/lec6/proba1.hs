
import Prelude

data Complex = Complex Double Double deriving (Show, Eq)

addComplex :: Complex -> Complex -> Complex
addComplex (Complex a b) (Complex c d) = Complex (a + c) (b + d)


subtractComplex :: Complex -> Complex -> Complex
subtractComplex (Complex a b) (Complex c d) = Complex (a - c) (b - d)


multiplyComplex :: Complex -> Complex -> Complex
multiplyComplex (Complex a b) (Complex c d) = Complex (a * c - b * d) (a * d + b * c)


divideComplex :: Complex -> Double -> Complex
divideComplex (Complex a b) k = Complex (a / k) (b / k)


sqrtComplex :: Double -> Complex
sqrtComplex x
    | x >= 0    = Complex (sqrt x) 0
    | otherwise = Complex 0 (sqrt (-x))


calculateEigenvalues :: Double -> Double -> Double -> Double -> (Complex, Complex)
calculateEigenvalues a b c d = (eigenvalue1, eigenvalue2)
  where
    trace = a + d
    determinant = a * d - b * c
    discriminant = trace^2 - 4 * determinant
    sqrtDiscriminant = sqrtComplex discriminant
    
    -- Calculate eigenvalues
    eigenvalue1 = divideComplex (addComplex (Complex trace 0) sqrtDiscriminant) 2
    eigenvalue2 = divideComplex (subtractComplex (Complex trace 0) sqrtDiscriminant) 2


extractMatrixElements :: [[Double]] -> (Double, Double, Double, Double, Double, Double, Double, Double, Double)
extractMatrixElements matrix = (a, b, c, d, e, f, g, h, i)
  where
    a = matrix !! 0 !! 0
    b = matrix !! 0 !! 1
    c = matrix !! 0 !! 2
    d = matrix !! 1 !! 0
    e = matrix !! 1 !! 1
    f = matrix !! 1 !! 2
    g = matrix !! 2 !! 0
    h = matrix !! 2 !! 1
    i = matrix !! 2 !! 2
    


calculateEigenvalues_3 :: [[Double]] -> (Complex, Complex, Complex)
calculateEigenvalues_3 matrix = 
    let (a, b, c, d, e, f, g, h, i) = extractMatrixElements matrix
        detA = a * e * i + d * h * c + g * b * f - g * e * c - d * b * i - a * h * f
        viet_a = - (a + e + i)
        viet_b = a * e + a * i - b * d - c * g + e * i - f * h
        viet_c = - detA
        viet_q = (viet_a^2 - 3*viet_b) / 9
        viet_r = (2 * viet_a^3 - 9 * viet_a * viet_b + 27 * viet_c) / 54
        viet_s = viet_q^3 - viet_r^2
    in if viet_s > 0 then
        --  viet_s > 0
        let phi = 1 / 3 * acos (viet_r / sqrt (viet_q^3))
            eigenvalue1 = Complex (- (2 * sqrt viet_q * cos phi) - viet_a / 3) 0
            eigenvalue2 = Complex (- (2 * sqrt viet_q * cos (phi - 2 * pi / 3)) - viet_a / 3) 0
            eigenvalue3 = Complex (- (2 * sqrt viet_q * cos (phi + 2 * pi / 3)) - viet_a / 3) 0
        in (eigenvalue1, eigenvalue2, eigenvalue3)
    else if viet_s < 0 then
        -- viet_s < 0
        if viet_q > 0 then
            -- viet_q > 0
            let phi = 1 / 3 * acosh (abs viet_r / sqrt (viet_q^3))
                eigenvalue1 = Complex (- ( 2 * signum viet_r * sqrt viet_q * cosh phi) - viet_a / 3) 0
                eigenvalue2 = Complex (signum viet_r * sqrt viet_q  * cosh phi - viet_a / 3) (sqrt 3 * sqrt viet_q * sinh phi)
                eigenvalue3 = Complex (signum viet_r * sqrt viet_q  * cosh phi - viet_a / 3) (- (sqrt 3 * sqrt viet_q * sinh phi))
            in (eigenvalue1, eigenvalue2, eigenvalue3)
        else if viet_q < 0 then
            -- viet_q < 0
            let phi = 1 / 3 * asinh (abs viet_r / sqrt ( abs viet_q ^ 3))
                eigenvalue1 = Complex (- (2 * signum viet_r * sqrt(abs viet_q) * sinh phi) - viet_a / 3) 0
                eigenvalue2 = Complex (signum viet_r * sqrt( abs viet_q ) * sinh phi - viet_a / 3) (sqrt 3 * sqrt(abs viet_q) * cosh phi)
                eigenvalue3 = Complex (signum viet_r * sqrt( abs viet_q ) * sinh phi - viet_a / 3) (- (sqrt 3 * sqrt(abs viet_q ) * cosh phi))
            in (eigenvalue1, eigenvalue2, eigenvalue3)
        else -- viet_q == 0
            let eigenvalue1 = Complex (- (1 * (viet_c - viet_a^3 / 27) ** (1 / 3)) - viet_a / 3) 0
                ev1 = -(1 * (viet_c - viet_a^3 / 27) ** (1 / 3)) - viet_a / 3
                eigenvalue2 = Complex (-(viet_a + ev1)) (1 / 2 * sqrt (abs ((viet_a - 3 * ev1) * (viet_a + ev1) - 4 * viet_b)))
                eigenvalue3 = Complex (-(viet_a + ev1)) (- ( 1 / 2 * sqrt (abs ((viet_a - 3 * ev1) * (viet_a + ev1)) - 4 * viet_b)))
            in (eigenvalue1, eigenvalue2, eigenvalue3)
    else --  viet_s == 0
        let eigenvalue1 = Complex (- (2 * signum viet_r * sqrt viet_q) - viet_a / 3) 0
            eigenvalue2 = Complex (signum viet_r * sqrt viet_q - viet_a / 3) 0
            eigenvalue3 = Complex (signum viet_r * sqrt viet_q - viet_a / 3) 0
        in (eigenvalue1, eigenvalue2, eigenvalue3)

            
        


-- Example usage
main :: IO ()
main = do
    let a = 1
        b = 2
        c = -3
        d = 4
        matrix = [[-1, 12, 3], [-5, 7, -14], [1, -5, -9]]
    let (eigenvalue1, eigenvalue2) = calculateEigenvalues a b c d
    let (a, b, c) = calculateEigenvalues_3 matrix
    putStrLn $ "The eigenvalues are: " ++ show eigenvalue1 ++ " and " ++ show eigenvalue2
    putStrLn $ "The eigenvalues 3x3 are: " ++ show (a, b, c)
