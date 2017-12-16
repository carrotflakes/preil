(in-package :cl-user)
(defpackage preil.core
  (:use :cl
        :preil.util)
  (:export #:*world*
           #:make-world
           #:do-clause
           #:do-predicate
           #:add-clause
           #:add-predicate
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
  patterns) ; A list of (bound-variables free-variables function)


(defun add-clause (head body)
  (setf (world-clauses *world*)
        (append (world-clauses *world*)
                (list (cleanse-term (cons head body))))))

(defun add-predicate (head patterns)
  (let ((predicate (make-predicate :head head
                                   :patterns patterns)))
    (setf (world-predicates *world*)
          (append (world-predicates *world*)
                  (list predicate)))))


(defun exec (goals term)
  (when (null goals)
    (when *resolved-function*
      (funcall *resolved-function* term)))

  (let ((goal (pop goals)))

    (dolist (predicate (world-predicates *world*))
      (multiple-value-bind (matched bindings)
          (unify goal (predicate-head predicate))
        (when matched
          (loop
             for (bound-variables free-variables function) in (predicate-patterns predicate)
             for parameters = (sub bound-variables bindings)
             when (notany #'contains-variable-p parameters)
             do (apply function
                       (lambda (bindings*)
                         (let ((term-1 (sub free-variables bindings))
                               (term-2 (sub free-variables bindings*)))
                           (multiple-value-bind (matched bindings**)
                               (unify term-1 term-2)
                             (when matched
                               (let ((bindings (append bindings bindings**)))
                                 (exec (sub goals bindings)
                                       (sub term bindings)))))))
                       parameters)
               (return)))))

    (dolist (clause (world-clauses *world*))
      (multiple-value-bind (matched bindings) (unify goal (car clause))
        (when matched
          '(format t "!~a~%~a~%~a~%~a~%~%" term goal clause bindings)
          (setf bindings
                (append bindings
                        (loop
                           for variable in (collect-variables clause)
                           unless (assoc variable bindings)
                           collect (cons variable
                                         (if (string= variable *variable-prefix*)
                                             variable
                                             (gensym (symbol-name variable)))))))
          (exec (append (sub (cdr clause)
                             bindings)
                        (sub goals
                             bindings))
                (sub term bindings)))))))

(defun solve (term goals *resolved-function*)
  (unless *world*
    (error "Out of world"))
  (exec (sub goals '()) term)
  nil)
