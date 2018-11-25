(in-package :cl-user)
(defpackage preil
  (:use :cl
        :preil.util
        :preil.unify
        :preil.core)
  (:export #:initialize-memory
           #:make-world
           #:in-world
           #:with-world
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


(defun initialize-memory (size)
  (setf *memory* (make-memory size)
        *write-table* (make-write-table size)))


(defun in-world (world) ; TODO :based '(world-1 world-2 ...)
  (setf *world* world))

(defmacro with-world ((world) &body body)
  `(let ((*world* ,world))
     ,@body))

(defmacro <- (head &body body)
  `(add-clause *world* ',head ',body))

(defmacro %- (head &body patterns)
  `(add-predicate *world* ',head ',patterns))

(defmacro import-world (package)
  `(merge-world *world* (funcall (intern "GET-WORLD" ,package))))


(defmacro unwind-protect-memory (form)
  `(let ((write-table-pointer *write-table-pointer*))
     (unless (or *memory* *write-table*)
       (error "memory is not initialized"))
    (unwind-protect
         ,form
      (memory-rewind write-table-pointer))))

(defun %solve-1 (term clauses)
  (unwind-protect-memory
   (block solve-1
     (solve term clauses
            (lambda (result)
              (return-from solve-1 (values result t)))))))

(defun %solve-all (term clauses)
  (unwind-protect-memory
   (let ((result nil))
     (solve term clauses
            (lambda (term)
              (push term result)))
     (nreverse result))))


(defmacro solve-1 (term &body clauses)
  `(%solve-1 ',term (list ,@clauses)))

(defmacro solvep (&body clauses)
  `(%solve-1 t (list ,@clauses)))

(defmacro do-solve ((variables &body body) &rest clauses)
  (let ((arguments (gensym "ARGUMENTS")))
  `(solve ',variables (list ,@clauses)
          (lambda (,arguments)
            (destructuring-bind ,variables ,arguments
              ,@body)))))

(defmacro solve-all (term &body clauses)
  `(%solve-all ',term (list ,@clauses)))
