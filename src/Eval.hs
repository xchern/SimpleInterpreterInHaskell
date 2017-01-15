module Eval where

import Value
import Error

evalExpr :: Expr -> Either Error Value
evalExpr e = eval (Closure {exprC = e, envC = emptyEnv})

-- eval two expressions error if not both number
evalGetTwoNumber :: Error -> (Expr, Expr) -> Closure -> Either Error (Double ,Double)
evalGetTwoNumber err (a, b) c = do
  ar <- eval c{exprC = a}
  br <- eval c{exprC = b}
  case (ar, br) of
    (Number av, Number bv) -> Right (av, bv)
    _ -> Left err

-- Either Monad used here make evaluation no longer strict
eval :: Closure -> Either Error Value
eval c@(Closure env expr) = case expr of
  -- literal values
  (Lit x) -> Right x
  -- logic operations
  (Not x) -> do
    xr <- eval c{exprC = x}
    case xr of
      (Logic xv) -> Right $ Logic (not xv)
      _ -> Left $ TypeError "`not` should be called with bool"
  (And a b) -> do
    ar <- eval c{exprC = a}
    br <- eval c{exprC = b}
    case (ar, br) of
      (Logic av, Logic bv) -> Right $ Logic (av && bv)
      _ -> Left $ TypeError "`and` should be called with bool"
  (Or a b) -> eval c{exprC = (Not (And (Not a) (Not b)))}
-- Numeric Operations
  (Sum a b) -> evalGetTwoNumber (TypeError "`+` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Number (av + bv))
  (Difference a b) -> evalGetTwoNumber (TypeError "`-` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Number (av - bv))
  (Product a b) -> evalGetTwoNumber (TypeError "`*` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Number (av * bv))
  (Quotinant a b) -> evalGetTwoNumber (TypeError "`/` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Number (av / bv))
-- Numeric Comparation Operation
  (Equal a b) -> evalGetTwoNumber (TypeError "`=` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Logic (av == bv))
  (Less a b) -> evalGetTwoNumber (TypeError "`<` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Logic (av < bv))
  (LessEqual a b) -> evalGetTwoNumber (TypeError "`<=` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Logic (av <= bv))
  (Greater a b) -> evalGetTwoNumber (TypeError "`>` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Logic (av > bv))
  (GreaterEqual a b) -> evalGetTwoNumber (TypeError "`>=` should be called with number") (a, b) c
               >>= (\(av, bv) -> Right $ Logic (av >= bv))
-- List Operation
  (Cons a b) -> do
    ar <- eval c{exprC = a}
    br <- eval c{exprC = b}    
    Right $ Pair ar br
  (Car p) -> do
    pr <- eval c{exprC = p}
    case pr of
      (Pair car _) -> Right car
      _ -> Left $ TypeError "`car` should be called with pairs"
  (Cdr p) -> do
    pr <- eval c{exprC = p}
    case pr of
      (Pair _ cdr) -> Right cdr
      _ -> Left $ TypeError "`cdr` should be called with pairs"
-- Functional
  (Atom a) -> Right $ lookupEnv a env
  (Lambda a e) -> Right $ FunctionV $ Function{paramF = a, closureF = c{exprC = e}} -- save environment
  (Apply func paramExpr) -> do
    fr <- eval c{exprC = func}
    case fr of
      FunctionV f -> do
        paramV <- eval c{exprC = paramExpr}
        eval (closureF f){envC = insertBind (paramF f) paramV (envC $ closureF f)}
      _ -> Left $ TypeError "try to call a variable which is not function"
  -- conditional expression
  -- `False` and `Nil` Treated as falsy value, others as true
  (If cond t f) -> do
    condr <- eval c{exprC = cond}
    case condr of
      Logic False -> eval c{exprC = f}
      Nil -> eval c{exprC = f}
      _ -> eval c{exprC = t}
