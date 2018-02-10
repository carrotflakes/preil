(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(print 1)
(let ((world (create-world
               (%- (add ?x ?y ?z)
                 ((?x ?y ?z)
                   (when (= (+ ?x ?y) ?z)
                     (satisfy)))))))

  (print (solvep world
           '(add 1 2 3))) ; => t
  (print (solvep world
           '(add 1 2 4))) ; => nil
  (print (solve-all world ?x
           '(add ?x 2 3))) ; => nil
  (print (solve-all world ?y
           '(add 1 ?y 3))) ; => nil
  (print (solve-all world ?z
           '(add 1 2 ?z)))) ; => nil

(print 2)
(let ((world (create-world
               (%- (add ?x ?y ?z)
                 ((?x ?y)
                   (satisfy :?z (+ ?x ?y)))))))

  (print (solvep world
           '(add 1 2 3)))
  (print (solvep world
           '(add 1 2 4)))
  (print (solve-all world ?x
           '(add ?x 2 3)))
  (print (solve-all world ?y
           '(add 1 ?y 3)))
  (print (solve-all world ?z
           '(add 1 2 ?z))))

(print 3)
(let ((world (create-world
               (%- (add ?x ?y ?z)
                 ((?x ?y ?z)
                   (when (= (+ ?x ?y) ?z)
                     (satisfy)))
                 ((?x ?y)
                   (satisfy :?z (+ ?x ?y)))))))

  (print (solvep world
           '(add 1 2 3)))
  (print (solvep world
           '(add 1 2 4)))
  (print (solve-all world ?x
           '(add ?x 2 3)))
  (print (solve-all world ?y
           '(add 1 ?y 3)))
  (print (solve-all world ?z
           '(add 1 2 ?z))))

(print 4)
(let ((world (create-world
               (%- (range ?x ?y)
                 ((?x)
                   (dotimes (i ?x)
                     (satisfy :?y i))))
               (%- (mod ?x ?y ?z)
                 ((?x ?y)
                   (satisfy :?z (mod ?x ?y)))))))

  (print (solve-all world ?x
           '(range 10 ?x)))
  (print (solve-all world ?y
           '(range 10 ?x)
           '(mod ?x 3 ?y))))

(print 5)
(let ((world (create-world
               (%- (eval ?exp ?val)
                 ((?exp)
                   (satisfy :?val (eval ?exp)))))))

  (print (solve-all world ?x
           '(eval (+ 1 (* 2 3)) ?x))))

(print 6)
(defvar fruits '(apple orange grape))
(let ((world (create-world
               (%- (fruit ?x)
                 (() (loop
                       for fruit in fruits
                       do (satisfy :?x fruit)))
                 ((?x) (when (member ?x fruits) (satisfy)))))))

  (print (solve-all world ?x
           '(fruit ?x)))
  (print (solvep world
           '(fruit apple)))
  (print (solve-all world ?x
           '(fruit pineapple))))
