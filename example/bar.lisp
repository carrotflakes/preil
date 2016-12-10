(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(print 1)
(with-world ()
  (%p (add ?x ?y ?z)
    (= (+ ?x ?y) ?z))

  (print (solvep
           '(add 1 2 3)))
  (print (solvep
           '(add 1 2 4)))
  (print (solve-all ?x
           '(add ?x 2 3)))
  (print (solve-all ?y
           '(add 1 ?y 3)))
  (print (solve-all ?z
           '(add 1 2 ?z))))

(print 2)
(with-world ()
  (%g (add ?x ?y ?z)
      (?x ?y)
    (ret :?z (+ ?x ?y)))

  (print (solvep
           '(add 1 2 3)))
  (print (solvep
           '(add 1 2 4)))
  (print (solve-all ?x
           '(add ?x 2 3)))
  (print (solve-all ?y
           '(add 1 ?y 3)))
  (print (solve-all ?z
           '(add 1 2 ?z))))

(print 3)
(with-world ()
  (%p (add ?x ?y ?z)
    (= (+ ?x ?y) ?z))

  (%g (add ?x ?y ?z)
      (?x ?y)
      (ret :?z (+ ?x ?y)))

  (print (solvep
           '(add 1 2 3)))
  (print (solvep
           '(add 1 2 4)))
  (print (solve-all ?x
           '(add ?x 2 3)))
  (print (solve-all ?y
           '(add 1 ?y 3)))
  (print (solve-all ?z
           '(add 1 2 ?z))))

(print 3)
(with-world ()
  (%g (range ?x ?y)
      (?x)
      (dotimes (i ?x)
        (ret :?y i)))

  (print (solve-all ?x
           '(range 10 ?x))))

(print 4)
(with-world ()
  (%g (eval ?exp ?val)
      (?exp)
      (ret :?val (eval ?exp)))

  (print (solve-all ?x
           '(eval (+ 1 (* 2 3)) ?x))))


(print 5)
'(with-world ()
  (%g (1+ ?n ?m)
      (?n)
    (ret :?m (1+ ?n)))

  (%g (1+ ?n ?m)
      (?m)
    (ret :?n (1- ?m)))

  ;; list-len
  (<- (list-len () 0))
  (<- (list-len (_ . ?xs) ?m)
    (list-len ?xs ?n)
    (+1 ?n ?m))

  (print (solve-all ?x
           '(1+ 1 ?x)))
  (print (solve-all ?x
           '(1+ ?x 1)))
  (print (solve-all ?x
           '(list-len (1 2 3) ?x)))
  (print (solve-all ?x
           '(list-len ?x 3))))
