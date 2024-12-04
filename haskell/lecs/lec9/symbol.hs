-- Define a data type for symbolic expressions
data Expr
  = Const Double -- Constant
  | Var String -- Variable (e.g., "x")
  | Add Expr Expr -- Addition
  | Mul Expr Expr -- Multiplication
  | Div Expr Expr -- Division
  | Pow Expr Expr -- Power
  | Sin Expr -- Sine function
  | Cos Expr -- Cosine function
  | Exp Expr -- Exponential function
  | Log Expr -- Natural logarithm
  deriving (Show, Eq)

-- Symbolic differentiation
deriv :: Expr -> String -> Expr
deriv (Const _) _ = Const 0 -- Constant rule
deriv (Var x) v = if x == v then Const 1 else Const 0 -- Variable rule
deriv (Add f g) v = Add (deriv f v) (deriv g v) -- Sum rule
deriv (Mul f g) v = Add (Mul (deriv f v) g) (Mul f (deriv g v)) -- Product rule
deriv (Div f g) v = Div (Add (Mul (deriv f v) g) (Mul (Const (-1)) (Mul f (deriv g v)))) (Pow g (Const 2)) -- Quotient rule
deriv (Pow u v) v' =
  Mul
    (Pow u v)
    (Add (Mul (deriv v v') (Log u)) (Mul v (Div (deriv u v') u))) -- Chain rule for power
deriv (Sin f) v = Mul (Cos f) (deriv f v) -- Derivative of sine
deriv (Cos f) v = Mul (Mul (Const (-1)) (Sin f)) (deriv f v) -- Derivative of cosine
deriv (Exp f) v = Mul (Exp f) (deriv f v) -- Derivative of exponential
deriv (Log f) v = Div (deriv f v) f -- Derivative of logarithm

simplify :: Expr -> Expr
-- Simplify constants
simplify (Add (Const a) (Const b)) = Const (a + b)
simplify (Mul (Const a) (Const b)) = Const (a * b)
simplify (Div (Const a) (Const b)) = Const (a / b)
simplify (Pow (Const a) (Const b)) = Const (a ** b)

-- Identity and zero rules for addition and multiplication
simplify (Add f (Const 0)) = simplify f
simplify (Add (Const 0) f) = simplify f
simplify (Mul f (Const 1)) = simplify f
simplify (Mul (Const 1) f) = simplify f
simplify (Mul (Const 0) _) = Const 0
simplify (Mul _ (Const 0)) = Const 0

simplify (Add (Add (Const a) f) (Mul (Const b) g)) | g == f = simplify (Add (Const a) (Mul (Const (1.0 + b)) f)) 
simplify (Add (Add (Const a) f) (Mul (Const b) (Add (Const c) g))) = simplify ( Add (Add (Const a) (Mul (Const b) (Const c))) (Mul (Const (1.0 + b)) f))
simplify (Add (Add f (Const a)) (Mul (Const b) (Add (Const c) g))) = simplify ( Add (Add (Const a) (Mul (Const b) (Const c))) (Mul (Const (1.0 + b)) f))
simplify (Add (Add f (Const a)) (Mul (Const b) (Add g (Const c)))) = simplify ( Add (Add (Const a) (Mul (Const b) (Const c))) (Mul (Const (1.0 + b)) f))

simplify (Mul (Add (Mul f (Const a)) (Const b)) (Add g (Const c))) = simplify ( Add (Mul (Add (Mul f (Const a)) (Const b)) g) (Mul (Add (Mul f (Const a)) (Const b)) (Const c)))
-- (((x * 2.0) + 1.0) * (x + 1.0))

simplify (Mul f g) | f == g = simplify (Pow f (Const 2))
simplify (Mul (Mul f (Const a)) g) | g == f = simplify (Mul (Const a) (Pow f (Const 2)))
simplify (Mul (Mul (Const a) f) g) | g == f = simplify (Mul (Const a) (Pow f (Const 2)))
simplify (Mul (Mul (Const a) f) (Mul (Const b) g)) | g == f = simplify (Mul (Const (a*b)) (Pow f (Const 2)))
simplify (Mul (Mul f (Const a) ) (Mul (Const b) g)) | g == f = simplify (Mul (Const (a*b)) (Pow f (Const 2)))
simplify (Mul (Mul f (Const a) ) (Mul g (Const b))) | g == f = simplify (Mul (Const (a*b)) (Pow f (Const 2)))
simplify (Mul (Mul (Const a) f) (Mul g (Const b))) | g == f = simplify (Mul (Const (a*b)) (Pow f (Const 2)))


simplify (Mul (Add (Mul f (Const a)) (Const b)) g) | g == f = simplify (Add (Mul (Const (a)) (Pow f (Const 2))) (Mul (Const b) f))
simplify (Mul (Add (Mul (Const a) f) (Const b)) g) | g == f = simplify (Add (Mul (Const (a)) (Pow f (Const 2))) (Mul (Const b) f))
simplify (Mul (Add (Const b) (Mul (Const a) f)) g) | g == f = simplify (Add (Mul (Const (a)) (Pow f (Const 2))) (Mul (Const b) f))
simplify (Mul (Add (Const b) (Mul f (Const a))) g) | g == f = simplify (Add (Mul (Const (a)) (Pow f (Const 2))) (Mul (Const b) f))



-- Remove redundant addition and multiplication with 1
simplify (Mul (Const 1.0) f) = simplify f
simplify (Mul f (Const 1.0)) = simplify f


-- Remove redundant additions with 0
simplify (Add (Const 0) f) = simplify f
simplify (Add f (Const 0)) = simplify f
simplify (Add (Const 0.0) f) = simplify f
simplify (Add f (Const 0.0)) = simplify f

-- Simplify expression like Expr1 * (Expr2 / Expr3) to ExprF / Expr3 Where ExprF = Expr1 * Expr2
simplify (Mul f (Div b c)) = Div (Mul f b) c
simplify (Mul (Div b c) f) = Div (Mul f b) c


simplify (Mul f (Div (Const 1.0) g)) | f == g = Const 1.0
simplify (Mul (Div (Const 1.0) g) f) | f == g = Const 1.0

-- Logarithmic and exponential rules
simplify (Log (Exp f)) = simplify f
simplify (Exp (Log f)) = simplify f
simplify (Log (Const 1)) = Const 0
simplify (Exp (Const 0)) = Const 1
simplify (Exp (Const a)) = Const (exp a)

-- Trigonometric simplifications
simplify (Sin (Const x))
  | x == 0 || x == pi || x == -pi = Const 0
  | x == pi / 2 = Const 1
  | x == -(pi / 2) = Const (-1)
simplify (Cos (Const x))
  | x == 0 = Const 1
  | x == pi || x == -pi = Const (-1)
  | x == pi / 2 || x == -(pi / 2) = Const 0

-- Division simplifications
simplify (Div f (Const 1)) = simplify f
simplify (Div (Mul f g) g') | g == g' = simplify f
simplify (Div (Mul g f) g') | g == g' = simplify f

-- Power simplifications
simplify (Pow f (Const 0)) = Const 1
simplify (Pow f (Const 1)) = simplify f
simplify (Pow (Pow f (Const a)) (Const b)) =
  simplify (Pow f (Const (a * b))) -- (f^a)^b = f^(a*b)
simplify (Pow (Mul f g) (Const a)) =
  simplify (Mul (Pow f (Const a)) (Pow g (Const a))) -- (f*g)^a = f^a * g^a



simplify (Div (Mul (Pow f (Const a)) h) g) -- (f^a * h) / g
  | f == g = 
    if a == 1 then simplify h 
    else if a < 1 then simplify (Div h (Pow g (Const (1 - a)))) 
    else simplify (Mul (Pow f (Const (a - 1))) h) -- (f^a * h) / f = f^(a-1) * h

simplify (Div (Mul h (Pow f (Const a))) (Pow g (Const b))) | f == g =  -- h * (f^a) / (g^b)
  if a > b then simplify (Mul (Pow f (Const (a - b))) h) -- h * (f^a) / (g^b) = f^(a-b) * h
  else if a == b then simplify h -- h * (f^a) / (g^a) = h / f
  else simplify (Div h (Pow g (Const (b - a)))) -- h * (f^a) / (g^b) = h / g^(b-a)

simplify (Div (Pow f (Const a)) (Pow g (Const b))) | f == g = -- (f^a) / (g^b)
  if a == b then Const 1.0
  else if a > b then simplify (Pow f (Const (a - b))) -- (f^a) / (g^b) = f^(a-b)
  else simplify (Div (Const 1.0) (Pow g (Const (b - a)))) -- (f^a) / (g^b) = 1 / g^(b-a)

simplify (Div (Pow f (Const a)) (Mul (Pow g (Const b)) h)) | f == g = -- (f^a) / (g^b * h)
  if a == b then simplify (Div (Const 1.0) h) -- (f^a) / (g^b * h) = 1 / h
  else if a > b then simplify (Div (Pow f (Const (a - b))) h) -- (f^a) / (g^b * h) = f^(a-b) / h
  else simplify (Div (Const 1.0) (Mul (Pow g (Const (b - a))) h)) -- (f^a) / (g^b * h) = 1 / (g^(b-a) * h)

simplify (Div (Mul (Pow f (Const a)) h) (Mul (Pow g (Const b)) h')) | f == g = -- (f^a * h) / (g^b * h')
  if a == b then simplify (Div h h') -- (f^a * h) / (g^b * h') = h / h'
  else if a > b then simplify (Div (Mul (Pow f (Const (a - b))) h) h') -- (f^a * h) / (g^b * h') = (f^(a-b) * h) / h'
  else simplify (Div h (Mul (Pow g (Const (b - a))) h')) -- (f^a * h) / (g^b * h') = h / (g^(b-a) * h')

  -- (((x + 1.0) ^ 2.0) * (2.0 * (1.0 / (x + 1.0))))
-- Expression 2: 
--   ((x + 1.0) ^ 2.0)
-- Derivative w.r.t x: 

--   ((((x + 1.0) ^ 2.0) * 2.0) / (x + 1.0))
-- Simplify expressions like x * (1.0 / x) to 1.0

-- Recursive application of simplifications
simplify (Add f g) = Add (simplify f) (simplify g)
simplify (Mul f g) = Mul (simplify f) (simplify g)
simplify (Div f g) = Div (simplify f) (simplify g)
simplify (Pow f g) = Pow (simplify f) (simplify g)
simplify (Sin f) = Sin (simplify f)
simplify (Cos f) = Cos (simplify f)
simplify (Exp f) = Exp (simplify f)
simplify (Log f) = Log (simplify f)
simplify expr = expr

simplifyExpr :: Expr -> Expr  
simplifyExpr expr = 
  let simplified = simplify expr
  in if simplified == expr then expr else simplifyExpr simplified
            

-- Pretty-print symbolic expressions
prettyPrint :: Expr -> String
prettyPrint (Const c) = show c
prettyPrint (Var x) = x
prettyPrint (Add f g) = "(" ++ prettyPrint f ++ " + " ++ prettyPrint g ++ ")"
prettyPrint (Mul f g) = "(" ++ prettyPrint f ++ " * " ++ prettyPrint g ++ ")"
prettyPrint (Div f g) = "(" ++ prettyPrint f ++ " / " ++ prettyPrint g ++ ")"
prettyPrint (Pow f g) = "(" ++ prettyPrint f ++ " ^ " ++ prettyPrint g ++ ")"
prettyPrint (Sin f) = "sin(" ++ prettyPrint f ++ ")"
prettyPrint (Cos f) = "cos(" ++ prettyPrint f ++ ")"
prettyPrint (Exp f) = "exp(" ++ prettyPrint f ++ ")"
prettyPrint (Log f) = "ln(" ++ prettyPrint f ++ ")"

-- Example usage
-- Example usage
main :: IO ()
main = do
  let expr = Div (Add (Pow (Var "x") (Const 2)) (Add (Var "x") (Const 1))) (Add (Var "x") (Const 1)) -- (x^2 + x + 1) / (x + 1)
  let expr0 = Div (Var "x") (Add (Const 1.0) (Var "x")) -- x / (1.0 + x)
  let expr1 = Pow (Var "x") (Var "x") -- x^x
  let expr2 = Pow (Add (Var "x") (Const 1)) (Const 2) -- (x + 1)^2
  let expr3 = Pow (Sin (Var "x")) (Sin (Var "x")) -- (e^x)^x
  let expr4 = Add (Var "x") ( Const 1) -- x + 0.0
  let expr5 = Div (Add (Var "x") ( Const 1)) (Add (Var "x") ( Const 2))
  let expr6 = Div (Add (Pow (Var "x") (Const 2)) (Add (Var "x") (Const 1))) (Add (Var "x") (Const 1)) -- (x^2 + x + 1) / (x + 1)

  putStrLn "Expression -1: "
  putStrLn $ "  " ++ prettyPrint expr
  putStrLn "simplified Expression -1: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr expr)

  -- Expression 0
  putStrLn "Expression 0: "
  putStrLn $ "  " ++ prettyPrint expr0
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr0 "x"))

  -- Expression 1
  putStrLn "\nExpression 1: "
  putStrLn $ "  " ++ prettyPrint expr1
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr1 "x"))
  

  -- Expression 2
  putStrLn "\nExpression 2: "
  putStrLn $ "  " ++ prettyPrint expr2
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint ( simplifyExpr (deriv expr2 "x"))


  -- Expression 3
  putStrLn "\nExpression 3: "
  putStrLn $ "  " ++ prettyPrint expr3
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr3 "x"))

  putStrLn "\nExpression 4: "
  putStrLn $ "  " ++ prettyPrint expr4
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr4 "x"))

  putStrLn "\nExpression 5: "
  putStrLn $ "  " ++ prettyPrint expr5
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr5 "x"))

  putStrLn "\nExpression 6: "
  putStrLn $ "  " ++ prettyPrint expr6
  putStrLn "Derivative w.r.t x: "
  putStrLn $ "  " ++ prettyPrint (simplifyExpr (deriv expr6 "x"))

