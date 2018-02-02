(in-package :cl-user)
(defpackage preil.util
  (:use :cl)
  (:export #:*variable-prefix*
           #:eq*
           #:variablep
           #:contains-variable-p
           #:collect-variables
           #:sub
           #:variables-cleanse-bindings
           #:cleanse-term
           #:unify))
(in-package :preil.util)


(declaim (inline *variable-prefix*))
(defvar *variable-prefix* "?")


(declaim (inline eq*))
(defun eq* (l r)
	(if (stringp l)
			(and (stringp r) (string= l r))
      (eq l r)))

(declaim (inline variablep))
(defun variablep (object)
  (and (symbolp object)
       (string= object *variable-prefix* :end1 1 :end2 1)))

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
     (if (string/= term *variable-prefix*)
         (let ((pair (assoc term bindings)))
           (if pair
               (sub (cdr pair) bindings)
               term))
         (gensym *variable-prefix*)))
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
     when (string/= variable *variable-prefix*)
     collect (cons variable (gensym (symbol-name variable)))))

(defun cleanse-term (term)
  (sub term
       (variables-cleanse-bindings (collect-variables term))))



(defvar *unified* nil)


(defun unified-value (term)
  (loop
     while (variablep term)
     for pair = (assoc term *unified*)
     while pair
     do (setf term (cdr pair)))
  term)

(defun %unify (term1 term2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  '(format t "~a ~a~%" term1 term2)

  (setf term1 (unified-value term1)
        term2 (unified-value term2))

  (cond
    ((eq* term1 term2)
     t)
    ((variablep term1)
     (unless (string= term1 *variable-prefix*)
       (push (cons term1 term2) *unified*))
     t)
    ((variablep term2)
     (unless (string= term2 *variable-prefix*)
       (push (cons term2 term1) *unified*))
     t)
    ((and (consp term1) (consp term2))
     (and (%unify (car term1) (car term2))
          (%unify (cdr term1) (cdr term2))))))

(defun unify (term1 term2 &aux (*unified* ()))
  (values (%unify term1 term2) *unified*))
