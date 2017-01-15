import Value
import Eval

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented"
    putStrLn $ "Fixed-point Combinator:\n" ++ show y
    putStrLn $ "fac 10 = " ++ show (evalExpr (Apply fac (Lit (Number 10))))

y,facProto,fac :: Expr

-- church's fixed combinator 
y' = (Lambda "f"
            (Apply
            (Lambda "x"
                    (Apply
                    (Atom "f")
                    (Apply (Atom "x") (Atom "x"))))
            (Lambda "x"
                    (Apply
                    (Atom "f")
                    (Apply (Atom "x") (Atom "x"))))))

-- strict fixed-point combinator
y = (Lambda "g" (Apply
                 (Lambda "f" (Apply (Atom "f") (Atom "f")))
                 (Lambda "f" (Apply
                              (Atom "g")
                              (Lambda "x" (Apply
                                           (Apply (Atom "f") (Atom "f"))
                                           (Atom "x")))))))


facProto = (Lambda "f"
              (Lambda "n"
               (If (Equal (Lit (Number 0)) (Atom "n")) (Lit (Number 1))
                   (Product (Atom "n") (Apply (Atom "f") (Difference (Atom "n") (Lit (Number 1))))))))

fac = (Apply y facProto)
