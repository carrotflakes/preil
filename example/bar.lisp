(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(print 1)
(with-world ()
  (%- (add ?x ?y ?z)
      ((?x ?y ?z)
       (when (= (+ ?x ?y) ?z)
         (satisfy))))

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
  (%- (add ?x ?y ?z)
      ((?x ?y)
       (satisfy :?z (+ ?x ?y))))

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
  (%- (add ?x ?y ?z)
      ((?x ?y ?z)
       (when (= (+ ?x ?y) ?z)
         (satisfy)))
      ((?x ?y)
       (satisfy :?z (+ ?x ?y))))

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

(print 4)
(with-world ()
  (%- (range ?x ?y)
      ((?x)
       (dotimes (i ?x)
         (satisfy :?y i))))

  (print (solve-all ?x
           '(range 10 ?x))))

(print 5)
(with-world ()
  (%- (eval ?exp ?val)
      ((?exp)
       (satisfy :?val (eval ?exp))))

  (print (solve-all ?x
           '(eval (+ 1 (* 2 3)) ?x))))


(print 6)
'(with-world ()
  (%- (1+ ?n ?m)
   ((?n)
    (satisfy :?m (1+ ?n)))
   ((?m)
    (satisfy :?n (1- ?m))))

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
