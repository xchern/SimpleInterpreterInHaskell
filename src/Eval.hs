module Eval where

import Value
import Data.Maybe

evalExpr :: Expr -> Value
evalExpr e = eval (Closure {exprC = e, envC = emptyEnv})

eval :: Closure -> Value
eval c@(Closure env expr) = case expr of
  (Lit x) -> x
  (Not x) -> let (Logic xv) = eval c{exprC = x} in
               (Logic (not xv))
-- eval (Not (Lit (Logic True)))
  (And a b) -> let (Logic av) = eval c{exprC = a}
                   (Logic bv) = eval c{exprC = b} in
                 Logic (av && bv)
-- eval (And (Lit (Logic True)) (Lit (Logic False)))
  (Or a b) -> eval c{exprC = (Not (And (Not a) (Not b)))}
-- eval (Or (Lit (Logic True)) (Lit (Logic False)))
-- Numeric Operations
  (Sum a b) -> let (Number av) = eval c{exprC = a}
                   (Number bv) = eval c{exprC = b} in
                 (Number (av + bv))
  (Difference a b) -> let (Number av) = eval c{exprC = a}
                          (Number bv) = eval c{exprC = b} in
                        (Number (av - bv))
  (Product a b) -> let (Number av) = eval c{exprC = a}
                       (Number bv) = eval c{exprC = b} in
                     (Number (av * bv))
  (Quotinant a b) -> let (Number av) = eval c{exprC = a}
                         (Number bv) = eval c{exprC = b} in
                       (Number (av / bv))
-- Numeric Comparation Operation
  (Equal a b) -> let (Number av) = eval c{exprC = a}
                     (Number bv) = eval c{exprC = b} in
                   (Logic (av == bv))
  (Less a b) -> let (Number av) = eval c{exprC = a}
                    (Number bv) = eval c{exprC = b} in
                  (Logic (av < bv))
  (LessEqual a b) -> let (Number av) = eval c{exprC = a}
                         (Number bv) = eval c{exprC = b} in
                       (Logic (av <= bv))
  (Greater a b) -> let (Number av) = eval c{exprC = a}
                       (Number bv) = eval c{exprC = b} in
                     (Logic (av > bv))
  (GreaterEqual a b) -> let (Number av) = eval c{exprC = a}
                            (Number bv) = eval c{exprC = b} in
                          (Logic (av >= bv))
-- List Operation
  (Cons a b) -> (Pair (eval c{exprC = a}) (eval c{exprC = b}))
  (Car p) -> let (Pair car _) = eval c{exprC = p} in car
  (Cdr p) -> let (Pair _ cdr) = eval c{exprC = p} in cdr
-- Functional
  (Atom a) -> fromJust (lookupEnv a env)
  (Lambda a e) -> FunctionV $ Function{paramF = a, closureF = c{exprC = e}} -- save environment
  (Apply func paramExpr) -> let (FunctionV f) = eval c{exprC = func}
                                paramV = eval c{exprC = paramExpr} in
                              eval (closureF f){envC = insertBind (paramF f) paramV (envC $ closureF f)}
  (If cond tv fv) -> let (Logic cv) = eval c{exprC = cond} in
                       if cv then eval c{exprC = tv} else eval c{exprC = fv}
