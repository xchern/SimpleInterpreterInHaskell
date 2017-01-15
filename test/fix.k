(let comment "
basic functional programing feature demo using fixed-combinator.
Will get `(1.0 . (2.0 . (6.0 . (24.0 . (120.0 . (720.0 . (5040.0 . (40320.0 . (362880.0 . (3628800.0 . nil))))))))))`
" nil)
(let y
    (lambda f ((lambda x (f (lambda v ((x x) v)))) (lambda x (f (lambda v ((x x) v))))))
(let fac
    (y (lambda fac
         (lambda n
           (if (= n 0)
               1
               (* n (fac (- n 1)))))))
(let map
    (y (lambda map
         (lambda f
           (lambda l
             (if l
                 (cons (f (car l))
                       ((map f) (cdr l)))
                 nil)))))
((map fac) (cons 1
           (cons 2
           (cons 3
           (cons 4
           (cons 5
           (cons 6
           (cons 7
           (cons 8
           (cons 9
           (cons 10
           nil)))))))))))
)))
