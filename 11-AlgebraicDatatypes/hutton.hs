data Expr = Lit Integer
          | Add Expr Expr

-- 1. Write the "eval" function which reduces an expression to a final
-- sum:

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add left right) = eval left + eval right

-- 2. Write a printer for the expression

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add left right) =
  (printExpr left) ++ " + " ++ (printExpr right)


-- Test Data

testExpr = Add (Add (Lit 1) (Lit 2)) (Lit 3)
