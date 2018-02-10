(in-package :cl-user)
(defpackage preil.core
  (:use :cl
        :preil.util)
  (:export #:make-world
           #:add-clause
           #:add-predicate
           #:merge-world
           #:solve
           #:satisfy
           #:exec
           #:solve))
(in-package :preil.core)


(defvar *world* nil)
(defvar *resolved-function* nil)


(defstruct world
  (clauses nil)
  (predicates nil))

(defstruct predicate
  head
  patterns) ; A list of (bound-svars free-svars free-variables function)


(defun add-clause (world head body)
  (setf (world-clauses world)
        (append (world-clauses world)
          (list (cleanse-term (cons head body))))))

(defun add-predicate (world head patterns)
  (let* ((head-variables (collect-variables head))
         (bindings (variables-cleanse-bindings head-variables))
         (cleansed-head (sub head bindings))
         (built-patterns
          (mapcar
            #'(lambda (pattern)
                (destructuring-bind (bound-variables . body) pattern
                  (let* ((free-variables (set-difference head-variables bound-variables))
                         (satisfy-macro-definition
                          `(satisfy (&key ,@free-variables)
                             (list 'funcall '%satisfy
                                   (list 'list ,@free-variables)))))
                    (list
                      (sub bound-variables bindings)
                      (sub free-variables bindings)
                      free-variables
                      (eval `(lambda (%satisfy ,@bound-variables)
                               (macrolet (,satisfy-macro-definition)
                                 ,@body)))))))
            patterns))
         (predicate (make-predicate :head cleansed-head
                                    :patterns built-patterns)))
    (setf (world-predicates world)
          (append (world-predicates world)
            (list predicate)))))

(defun merge-world (world inner-world)
  (setf (world-clauses world)
        (append (world-clauses world)
                (world-clauses inner-world))
        (world-predicates world)
        (append (world-predicates world)
                (world-predicates inner-world))))


(defun exec (goals)
  (when (null (cdr goals))
    (when *resolved-function*
      (funcall *resolved-function* (first goals))))

  (let ((goal (pop goals)))

    (dolist (predicate (world-predicates *world*))
      (next-bound-id)
      (when (unify goal (predicate-head predicate))
        (loop
          for (bound-svars free-svars free-variables function) in (predicate-patterns predicate)
          for parameters = (ssub bound-svars)
          when (notany #'contains-svar-p parameters) ; If parameters contain svar, cannot apply the predicate.
          do (apply function
                    (lambda (term-2) ; term-2 contains no svar.
                      '(format t "~%!~a~% ~a~% ~a" term-1 term-2 bindings)
                      (next-bound-id)
                      (when (and (unify goal (predicate-head predicate)) ; XXX
                                 (unify free-svars term-2))
                        (exec (ssub goals))))
                    parameters)
             (return))))

    (dolist (clause (world-clauses *world*))
      (next-bound-id)
      (when (unify goal (car clause))
        '(format t "~%!~a~%~a~%~a~%" term goal clause)
        (bind-new-svar clause)
        (exec (ssub (append (cdr clause) goals)))))))

(defun solve (*world* term goals *resolved-function*)
  (exec (cleanse-term (append goals (list term))))
  nil)
