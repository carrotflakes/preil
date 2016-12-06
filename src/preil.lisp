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
  (clauses nil))

(defmacro with-world ((&optional (world (make-world))) &body body)
  `(let ((*world* ,world))
     ,@body))

(defun add-clause (head body)
  (setf (slot-value *world* 'clauses)
        (append
         (slot-value *world* 'clauses)
         (list (cons head body)))))

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
         term))
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
          (exec (sub (append (cdr clause) goals) bindings)
                (sub term bindings)))))))

(defun solve (term goals *resolved-function*)
  (exec goals term))

(defmacro -- (head &body body)
  `(add-clause ',head ',body))

(define-condition ?1-end (simple-error)
  ((result :initarg :result)))

(defmacro ?1 (term &body clauses)
  `(handler-case
       (solve ',term ',clauses
              (lambda (result)
                (error '?1-end :result result)))
     (?1-end (c)
       (slot-value c 'result))))

(defmacro ? (&body clauses)
  `(?1 t ,@clauses))

(defmacro ?do ((variables &body body) &rest clauses)
  `(solve ',variables ',clauses
          (lambda ,variables ,@body)))

(defmacro ?all (term &body clauses)
  (let ((result (gensym)))
    `(let ((,result nil))
       (solve ',term ',clauses
              (lambda (term)
                (push term ,result)))
       ,(nreverse result))))

(defmacro ?print (term &body clauses)
  `(solve ',term ',clauses #'print))
