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
  patterns) ; A list of (bound-variables free-variables function)


(defun add-clause (world head body)
  (setf (world-clauses world)
        (append (world-clauses world)
          (list (cleanse-term (cons head body))))))

(defun add-predicate (world head patterns)
  (let* ((head-variables (collect-variables head))
         (bindings (variables-cleanse-bindings head-variables))
         (cleansed-head (sub head bindings))
         (cleansed-patterns
          (mapcar
            #'(lambda (pattern)
                (destructuring-bind (bound-variables . body) pattern
                  (let* ((free-variables (set-difference head-variables bound-variables))
                         (satisfy-macro-definition
                          `(satisfy (&key ,@free-variables)
                             (list 'funcall '%satisfy
                                   (list 'list
                                         ,@(loop
                                             for variable in free-variables
                                             collect `(list 'cons
                                                            '',(sub variable bindings)
                                                        ,variable)))))))
                    (list
                      (sub bound-variables bindings)
                      (sub free-variables bindings)
                      (eval `(lambda (%satisfy ,@bound-variables)
                               (macrolet (,satisfy-macro-definition)
                                 ,@body)))))))
            patterns))
         (predicate (make-predicate :head cleansed-head
                                    :patterns cleansed-patterns)))
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

(defun solve (*world* term goals *resolved-function*)
  (exec (sub goals '()) term)
  nil)
