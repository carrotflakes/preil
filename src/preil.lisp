(in-package :cl-user)
(defpackage preil
  (:use :cl)
  (:export #:*world*
           #:make-world
           #:with-world
           #:solve
           #:--
           #:?
           #:?1
           #:?all
           #:?do
           #:?print))
(in-package :preil)


(defvar *world* nil)
(defvar *unified* nil)
(defvar *resolved-function* nil)


(defstruct world
  (clauses nil)
  (predicates nil)
  (parent nil))

(defmacro with-world ((&optional (world (make-world :parent *world*))) &body body)
  `(let ((*world* ,world))
     ,@body))

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

(defun cleanse-term (term)
  (let ((new-variable-alist
         (loop
            for variable in (collect-variables term)
            when (string/= variable "_")
            collect (cons variable (gensym (symbol-name variable))))))
    (labels ((f (term)
               (cond
                 ((variablep term)
                  (if (string= term "_")
                      (gensym "_")
                      (cdr (assoc term new-variable-alist))))
                 ((consp term)
                  (let ((car (f (car term)))
                        (cdr (f (cdr term))))
                    (if (and (eq car (car term))
                             (eq cdr (cdr term)))
                        term
                        (cons car cdr))))
                 (t
                  term))))
      (f term))))

(defun add-clause (head body)
  (setf (world-clauses *world*)
        (append (world-clauses *world*)
                (list (cleanse-term (cons head body))))))

(defun add-predicate (head function)
  (setf (world-predicates *world*)
        (append (world-predicates *world*)
                (list (cons (cleanse-term head) function)))))
#|
(%- (add x y _z)
    (+ x y))
(%m (add x y _z)
  (_z)
  (let ((a '(1 2 3 4)))
    (lambda ()
      (pop a))))

%p, %s, %g

(%p (add x y z)
  (= (+ x y) z))
(%s (add x y _z)
  (_z)
  (values (+ x y)))
(%g (add x y _z)
  (_z)
  (lambda ()
    (values 1)))
|#

#|
(defmacro %p (head &body body)
  `(add-predicate ,head
                  (lambda ,head
                    (lambda ()
                      (and (progn ,@body)
                           (values t))))))

(defmacro %s (head &body body)
  `(add-predicate ,head
                  (lambda ,head
                    (lambda ()
                      ,@body))))
|#

(defmacro do-clause ((clause) &body body)
  (let ((world (gensym)))
  `(loop
      with ,world = *world*
      while ,world
      do (dolist (,clause (world-clauses ,world))
           ,@body)
        (setf ,world (world-parent ,world)))))

(defun variablep (object)
  (and (symbolp object)
       (string= object "_" :end1 1 :end2 1)))

(defun unified-value (term)
  (if (variablep term)
      (let ((pair (assoc term *unified*)))
        (if pair
            (unified-value (cdr pair))
            term))
      term))

(defun %unify (term1 term2)
  '(format t "~a ~a~%" term1 term2)
  (setf term1 (unified-value term1)
        term2 (unified-value term2))

  (cond
    ((eq term1 term2)
     t)
    ((variablep term1)
     (when (string= term1 "_") (return-from %unify t))
     (push (cons term1 term2) *unified*)
     t)
    ((variablep term2)
     (when (string= term2 "_") (return-from %unify t))
     (push (cons term2 term1) *unified*)
     t)
    ((and (consp term1) (consp term2))
     (and (%unify (car term1) (car term2))
          (%unify (cdr term1) (cdr term2))))))

(defun unify (term1 term2 &aux (*unified* ()))
  (values (%unify term1 term2) *unified*))

(defun sub (term bindings)
  (cond
    ((variablep term)
     (if (string/= term "_")
         (let ((pair (assoc term bindings)))
           (if pair
               (sub (cdr pair) bindings)
               term))
         (gensym "_")))
    ((consp term)
     (let ((car (sub (car term) bindings))
           (cdr (sub (cdr term) bindings)))
       (if (and (eq car (car term))
                (eq cdr (cdr term)))
           term
           (cons car cdr))))
    (t
     term)))

(defun exec (goals term)
  (when (null goals)
    (when *resolved-function*
      (funcall *resolved-function* term)))

  (let ((goal (pop goals)))
    (dolist (clause (slot-value *world* 'clauses))
      (multiple-value-bind (matched bindings) (unify goal (car clause))
        (when matched
          '(format t "!~a~%~a~%~a~%~a~%~%" term goal clause bindings)
          (setf bindings
                (append bindings
                        (loop
                           for variable in (collect-variables (cdr clause))
                           unless (assoc variable bindings)
                           collect (cons variable
                                         (if (string= variable "_")
                                             variable!
                                             (gensym (symbol-name variable)))))))
          (exec (append (sub (cdr clause)
                             bindings)
                        (sub goals
                             bindings))
                (sub term bindings)))))))

(defun solve (term goals *resolved-function*)
  (exec goals term))

(defmacro -- (head &body body)
  `(add-clause ',head ',body))

(define-condition ?1-end (simple-error)
  ((result :initarg :result)))

(defmacro ?1 (term &body clauses)
  `(handler-case
       (solve ',term (list ,@clauses)
              (lambda (result)
                (error '?1-end :result result)))
     (?1-end (c)
       (slot-value c 'result))))

(defmacro ? (&body clauses)
  `(?1 't ,@clauses))

(defmacro ?do ((variables &body body) &rest clauses)
  `(solve ',variables (list ,@clauses)
          (lambda ,variables ,@body)))

(defmacro ?all (term &body clauses)
  (let ((result (gensym)))
    `(let ((,result nil))
       (solve ',term (list ,@clauses)
              (lambda (term)
                (push term ,result)))
       (nreverse ,result))))

(defmacro ?print (term &body clauses)
  `(solve ',term (list ,@clauses) #'print))
