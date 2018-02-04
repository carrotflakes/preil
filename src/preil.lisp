(in-package :cl-user)
(defpackage preil
  (:use :cl
        :preil.util
        :preil.core)
  (:export #:create-world
           #:<-
           #:%-
           #:import-world
           #:solve
           #:solvep
           #:%solve-1
           #:%solve-all
           #:solve-1
           #:solve-all
           #:do-solve
           #:satisfy))
(in-package :preil)


(defmacro create-world (&body body)
  (let ((world (gensym "WORLD")))
    `(let ((,world (make-world)))
       (macrolet
         ((<- (head &body body)
            (list 'add-clause ',world `',head `',body))
          (%- (head &body patterns)
            (list 'add-predicate ',world `',head `',patterns))
          (import-world (package)
            (list 'merge-world ',world `(funcall (intern "GET-WORLD" ,package)))))
         ,@body)
       ,world)))


(defun %solve-1 (world term clauses)
  (block solve-1
     (solve world term clauses
            (lambda (result)
              (return-from solve-1 (values result t))))))

(defmacro solve-1 (world term &body clauses)
  `(%solve-1 ,world ',term (list ,@clauses)))

(defmacro solvep (world &body clauses)
  `(%solve-1 ,world t (list ,@clauses)))

(defmacro do-solve (world (variables &body body) &rest clauses)
  (let ((arguments (gensym "ARGUMENTS")))
  `(solve ,world ',variables (list ,@clauses)
          (lambda (,arguments)
            (destructuring-bind ,variables ,arguments
              ,@body)))))

(defun %solve-all (world term clauses)
  (let ((result nil))
    (solve world term clauses
           (lambda (term)
             (push term result)))
    (nreverse result)))

(defmacro solve-all (world term &body clauses)
  `(%solve-all ,world ',term (list ,@clauses)))
