(in-package :cl-user)
(defpackage preil
  (:use :cl
        :preil.util
        :preil.core)
  (:export #:*world*
           #:make-world
           #:with-world
           #:<-
           #:%-
           #:solve
           #:solvep
           #:%solve-1
           #:%solve-all
           #:solve-1
           #:solve-all
           #:do-solve
           #:satisfy
           #:preil-defun))
(in-package :preil)


(defmacro with-world ((&optional (world '(make-world :parent *world*))) &body body)
  `(let ((*world* ,world))
     ,@body))

(defmacro preil-defun (name args &body body)
  (let ((world (gensym "WORLD")))
    `(let ((,world *world*))
       (defun ,name ,args
         (let ((*world* ,world))
           ,@body)))))

(defmacro <- (head &body body)
  `(add-clause ',head ',body))

(defmacro %- (head &body patterns)
  (let* ((head-variables (collect-variables head))
         (bindings (variables-cleanse-bindings head-variables))
         (cleansed-head (sub head bindings)))
    `(add-predicate
      ',cleansed-head
      (list ,@(loop
                 for (bound-variables . body) in patterns
                 for free-variables = (set-difference head-variables bound-variables)
                 for satisfy-macro = `(satisfy (&key ,@free-variables)
                                               (list 'funcall '%satisfy
                                                     (list 'list
                                                           ,@(loop
                                                                for variable in free-variables
                                                                collect `(list 'cons
                                                                               '',(sub variable bindings)
                                                                               ,variable)))))
                 collect `(list
                           ',(sub bound-variables bindings)
                           ',(sub free-variables bindings)
                           (lambda (%satisfy ,@bound-variables)
                             (macrolet (,satisfy-macro)
                               ,@body))))))))


(defun %solve-1 (term clauses)
  (block solve-1
     (solve term clauses
            (lambda (result)
              (return-from solve-1 (values result t))))))

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

(defun %solve-all (term clauses)
  (let ((result nil))
    (solve term clauses
           (lambda (term)
             (push term result)))
    (nreverse result)))

(defmacro solve-all (term &body clauses)
  `(%solve-all ',term (list ,@clauses)))
