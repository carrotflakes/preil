(in-package :cl-user)
(defpackage preil
  (:use :cl)
  (:export #:*world*
           #:make-world
           #:with-world
           #:solve
           #:<-
           #:solvep
           #:%solve-1
           #:%solve-all
           #:solve-1
           #:solve-all
           #:do-solve
           #:%-
           #:satisfy
           #:preil-defun))
(in-package :preil)


(defvar *world* nil)
(defvar *unified* nil)
(defvar *resolved-function* nil)


(defstruct world
  (clauses nil)
  (predicates nil)
  (parent nil))

(defstruct predicate
  head
  patterns) ; A list of (bound-variables free-variables function)

(defun eq* (l r)
	(if (stringp l)
			(and (stringp r) (string= l r))
      (eq l r)))

(defun variablep (object)
  (and (symbolp object)
       (string= object "?" :end1 1 :end2 1)))

(defun contains-variable-p (term)
  (labels ((f (term)
             (cond
               ((variablep term)
                t)
               ((consp term)
                (or (f (car term))
                    (f (cdr term)))))))
    (f term)))

(defun collect-variables (term)
  (let ((variables nil))
    (labels ((f (term)
               (cond
                 ((and (variablep term)
                       (not (member term variables)))
                  (push term variables))
                 ((consp term)
                  (f (car term))
                  (f (cdr term))))))
      (f term))
    variables))

(defun sub (term bindings)
  (cond
    ((variablep term)
     (if (string/= term "?")
         (let ((pair (assoc term bindings)))
           (if pair
               (sub (cdr pair) bindings)
               term))
         (gensym "?")))
    ((consp term)
     (let ((car (sub (car term) bindings))
           (cdr (sub (cdr term) bindings)))
       (if (and (eq* car (car term))
                (eq* cdr (cdr term)))
           term
           (cons car cdr))))
    (t
     term)))

(defun variables-cleanse-bindings (variables)
  (loop
     for variable in variables
     when (string/= variable "?")
     collect (cons variable (gensym (symbol-name variable)))))

(defun cleanse-term (term)
  (sub term
       (variables-cleanse-bindings (collect-variables term))))


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



(defmacro do-clause ((clause) &body body)
  (let ((world (gensym)))
  `(loop
      with ,world = *world*
      while ,world
      do (dolist (,clause (world-clauses ,world))
           ,@body)
        (setf ,world (world-parent ,world)))))

(defmacro do-predicate ((predicate) &body body)
  (let ((world (gensym)))
  `(loop
      with ,world = *world*
      while ,world
      do (dolist (,predicate (world-predicates ,world))
           ,@body)
        (setf ,world (world-parent ,world)))))

(defun unified-value (term)
  (if (variablep term)
      (let ((pair (assoc term *unified*)))
        (if pair
            (unified-value (cdr pair))
            term))
      term))

(defun %unify (term1 term2)
  '(format t "~a ~a~%" term1 term2)

  (labels ((unified-value (term)
             (if (variablep term)
                 (let ((pair (assoc term *unified*)))
                   (if pair
                       (unified-value (cdr pair))
                       term))
                 term)))
    (setf term1 (unified-value term1)
          term2 (unified-value term2)))

  (cond
    ((eq* term1 term2)
     t)
    ((variablep term1)
     (when (string= term1 "?") (return-from %unify t))
     (push (cons term1 term2) *unified*)
     t)
    ((variablep term2)
     (when (string= term2 "?") (return-from %unify t))
     (push (cons term2 term1) *unified*)
     t)
    ((and (consp term1) (consp term2))
     (and (%unify (car term1) (car term2))
          (%unify (cdr term1) (cdr term2))))))

(defun unify (term1 term2 &aux (*unified* ()))
  (values (%unify term1 term2) *unified*))

(defun exec (goals term)
  (when (null goals)
    (when *resolved-function*
      (funcall *resolved-function* term)))

  (let ((goal (pop goals)))

    (do-predicate (predicate)
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

    (do-clause (clause)
      (multiple-value-bind (matched bindings) (unify goal (car clause))
        (when matched
          '(format t "!~a~%~a~%~a~%~a~%~%" term goal clause bindings)
          (setf bindings
                (append bindings
                        (loop
                           for variable in (collect-variables (cdr clause))
                           unless (assoc variable bindings)
                           collect (cons variable
                                         (if (string= variable "?")
                                             variable
                                             (gensym (symbol-name variable)))))))
          (exec (append (sub (cdr clause)
                             bindings)
                        (sub goals
                             bindings))
                (sub term bindings)))))))

(defun solve (term goals *resolved-function*)
  (exec (sub goals '()) term)
  nil)


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


(defun %solve-1 (term clauses)
  (block solve-1
     (solve term clauses
            (lambda (result)
              (return-from solve-1 result)))))

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
