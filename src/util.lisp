(in-package :cl-user)
(defpackage preil.util
  (:use :cl)
  (:export #:+variable-char+
           #:eq*
           #:variablep
           #:contains-variable-p
           #:collect-variables
           #:sub
           #:variables-cleanse-bindings
           #:cleanse-term
           #:unify))
(in-package :preil.util)


(defconstant +variable-char+ "?")


(declaim (inline eq*))
(defun eq* (l r)
	(if (stringp l)
			(and (stringp r) (string= l r))
      (eq l r)))

(declaim (inline variablep))
(defun variablep (object)
  (and (symbolp object)
       (string= object +variable-char+ :end1 1 :end2 1)))

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
     (if (string/= term +variable-char+)
         (let ((pair (assoc term bindings)))
           (if pair
               (sub (cdr pair) bindings)
               term))
         (gensym +variable-char+)))
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
     when (string/= variable +variable-char+)
     collect (cons variable (gensym (symbol-name variable)))))

(defun cleanse-term (term)
  (sub term
       (variables-cleanse-bindings (collect-variables term))))



(defvar *unified* nil)


(defun unified-value (term)
  (if (variablep term)
      (let ((pair (assoc term *unified*)))
        (if pair
            (unified-value (cdr pair))
            term))
      term))

(defun %unify (term1 term2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
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
     (when (string= term1 +variable-char+) (return-from %unify t))
     (push (cons term1 term2) *unified*)
     t)
    ((variablep term2)
     (when (string= term2 +variable-char+) (return-from %unify t))
     (push (cons term2 term1) *unified*)
     t)
    ((and (consp term1) (consp term2))
     (and (%unify (car term1) (car term2))
          (%unify (cdr term1) (cdr term2))))))

(defun unify (term1 term2 &aux (*unified* ()))
  (values (%unify term1 term2) *unified*))
