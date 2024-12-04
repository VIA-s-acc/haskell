-- File: RK4.hs

import Text.Printf (printf)
import Data.List (scanl')

-- Define the type alias for the ODE function
type ODEFunc = Double -> Double -> Double

-- Runge-Kutta 4th Order Method
rk4 :: ODEFunc -> Double -> Double -> Double -> Double -> [(Double, Double)]
rk4 f x0 y0 h xn = takeWhile (\(x, _) -> x <= xn + 1e-10) (iterate step (x0, y0))
  where
    step (x, y) = (x + h, y + (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4))
      where
        k1 = f x y
        k2 = f (x + h / 2) (y + h / 2 * k1)
        k3 = f (x + h / 2) (y + h / 2 * k2)
        k4 = f (x + h)     (y + h * k3)

-- ODE function
f :: ODEFunc
f x y = y - x^2 + 1

-- Exact solution for comparison
exactSolution :: Double -> Double
exactSolution x = (x + 1)^2 - 0.5 * exp x


type Vector2D = (Double, Double) -- (y1, y2) -> ОДУ второго порядка переводим в 2 оду первого y1 = y, y2 = y` = dy1/dx
-- если наше уравнение y`` + e^(-y) = 0, y(0) = 1, y`(0) = 0,получаем dy2/dx = -e^(-y) и dy1/dx = y2, y1(0) = 1, y2(0) = 0
-- Main function to compute and display results

type ODESystem = Double -> Vector2D -> Vector2D

rk4Sys :: ODESystem -> Double -> Vector2D -> Double -> Double -> [(Double, Vector2D)]
rk4Sys system x0 y0 h xn = takeWhile (\(x, _) -> x <= xn + 1e-10) (iterate step (x0, y0))
  where
    step (x, (y1, y2)) =
      let k1 = system x (y1, y2)
          k2 = system (x + h / 2) (y1 + h / 2 * fst k1, y2 + h / 2 * snd k1)
          k3 = system (x + h / 2) (y1 + h / 2 * fst k2, y2 + h / 2 * snd k2)
          k4 = system (x + h) (y1 + h * fst k3, y2 + h * snd k3)
          dy1 = (h / 6) * (fst k1 + 2 * fst k2 + 2 * fst k3 + fst k4)
          dy2 = (h / 6) * (snd k1 + 2 * snd k2 + 2 * snd k3 + snd k4)
      in (x + h, (y1 + dy1, y2 + dy2))


-- Функция системы для уравнения y'' + e^(-y) = 0
system :: ODESystem
system _ (y1, y2) = (y2, -exp (-y1))

-- Точное аналитическое решение
y1Exact :: Double -> Double
y1Exact x = 1 + log (cos (x / sqrt (2 * exp 1)) ** 2)

y2Exact :: Double -> Double
y2Exact x = -sqrt (2 / exp 1) * tan (x / sqrt (2 * exp 1))

printResI :: Int -> [(Double, Vector2D)] -> IO ()
printResI n result =
  let (x, (y1, y2)) = result !! n
  in printf "%.5f\t%.6f\t%.6f\n" x y1 y2


main :: IO ()
main = do
  let x0 = 0            
      y0 = (1, 0)        
      h = 0.0001          
      xn = 4            
      result = rk4Sys system x0 y0 h xn

      resultWithErrors = scanl' (\(_, _, totalErr) (x, (y1, y2)) ->
                                   let exact1 = y1Exact x
                                       exact2 = y2Exact x
                                       localError = abs (y1 - exact1) + abs (y2 - exact2)
   
                                   in (x, (y1, y2), totalErr + localError) 
                                ) (x0, y0, 0 ) result


  putStrLn "x\t\t\t\ty\t|\ty'"
  -- let (x, (y1, y2)) = last result
  -- printf "%.3f\t%.6f\t%.6f\n" x y1 y2
  -- mapM_ (\(x, (y1, y2)) -> printf "%.5f\t%.6f\t%.6f\n" x y1 y2) result
  putStrLn "x\t|\ty\t\ty'\t|\tLocal Error\t\tError "
  let real = y1Exact 3.663000
  mapM_ (\(x, (y1, y2), totalError) -> do
         let exact1 = y1Exact x
             exact2 = y2Exact x
             localError = abs (y1 - exact1) + abs (y2 - exact2)
         printf "%.6f\t|\t%.6f\t%.6f\t|\tERROR = %.6e | LOCAL = %.6e\n" x y1 y2 totalError localError
      ) $ takeWhile (\(_, _, totalError) -> not (isInfinite totalError)) resultWithErrors
  printf "%.6f" real

  -- mapM_ (\(x, (y1, y2)) -> printf "%.5f\t%.6f\t|\t%.6f\n" x y1 y2) $
  --     takeWhile (\(_, (y1, y2)) -> not (isInfinite y1 || isInfinite y2)) result


-- 0.79667 0.880912        -0.305043 
-- 1.59983 0.486854        -0.702392
-- 2.16923 -0.029806       -1.150978
-- 2.69696 -0.820700       -1.951517
-- 3.11694 -1.923179       -3.598554