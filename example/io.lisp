(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(solvep
  (create-world
    (%- (read ?x)
      (()
       (satisfy :?x (read-line))))
    (%- (write ?x)
      ((?x)
       (format t "~a~%" ?x)
       (satisfy)))
    (%- repeat
      (()
       (loop (satisfy))))

    (<- (= ?x ?x))

    (<- run
      (write "Please say anything!")
      repeat
      (read ?x)
      (write "You say:")
      (write ?x)
      (= ?x "bye")
      (write "Bye!"))
    )
  'run)
