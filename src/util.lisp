(in-package :cl-user)
(defpackage preil.util
  (:use :cl)
  (:export #:*variable-prefix*
           #:make-svar
           #:svar-name
           #:svar-p
           #:eq*
           #:variablep
           #:contains-svar-p
           #:collect-variables
           #:collect-svars
           #:sub
           #:ssub
           #:variables-cleanse-bindings
           #:cleanse-term
           #:unify))
(in-package :preil.util)


(declaim (inline *variable-prefix*))
(defvar *variable-prefix* "?")


(defstruct svar
  name)

(defmethod print-object ((obj svar) out)
  #+sbcl(format out "#~a~a" (svar-name obj)
          (mod (sb-kernel:get-lisp-obj-address obj) 10000))
  #-sbcl(format out "#~a" (svar-name obj)))


(declaim (inline eq*))
(defun eq* (l r)
	(if (stringp l)
			(and (stringp r) (string= l r))
      (eq l r)))

(declaim (inline variablep))
(defun variablep (object)
  (and (symbolp object)
       (string= object *variable-prefix* :end1 1 :end2 1)))

;; (declaim (inline wildcard-p))
;; (defun wildcard-p (svar)
;;   (string= *variable-prefix* (svar-name svar)))

(defun contains-svar-p (term)
  (labels ((f (term)
             (cond
               ((svar-p term)
                t)
               ((consp term)
                (or (f (car term))
                    (f (cdr term)))))))
    (f term)))

(defun collect-variables (term &optional variables)
  (labels ((f (term)
             (cond
               ((and (variablep term)
                     (not (member term variables)))
                (push term variables))
               ((consp term)
                (f (car term))
                (f (cdr term))))))
    (f term))
  variables)

(defun collect-svars (term &optional svars)
  (labels ((f (term)
             (cond
               ((and (svar-p term)
                     (not (member term svars)))
                (push term svars))
               ((consp term)
                (f (car term))
                (f (cdr term))))))
    (f term))
  svars)

(defun sub (term bindings)
  (cond
    ((variablep term)
     (if (string= term *variable-prefix*)
         (make-svar :name "!");*variable-prefix*)
         (let ((pair (assoc term bindings)))
           (if pair
               (sub (cdr pair) bindings)
               term))))
    ((consp term)
     (let ((car (sub (car term) bindings))
           (cdr (sub (cdr term) bindings)))
       (if (and (eq* car (car term))
                (eq* cdr (cdr term)))
           term
           (cons car cdr))))
    (t
     term)))

(defun ssub (term bindings)
  (cond
    ((svar-p term)
      (let ((pair (assoc term bindings)))
        (if pair
            (ssub (cdr pair) bindings)
            term)))
    ((consp term)
     (let ((car (ssub (car term) bindings))
           (cdr (ssub (cdr term) bindings)))
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
     collect (cons variable (make-svar :name (symbol-name variable)))))

(defun cleanse-term (term)
  (sub term
       (variables-cleanse-bindings (collect-variables term))))


(declaim (inline unified-value))
(defun unified-value (unified term)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop
     while (svar-p term)
     for pair = (assoc term unified)
     while pair
     do (setf term (cdr pair)))
  term)

(defun %unify (unified term1 term2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  '(format t "~a ~a~%" term1 term2)

  (setf term1 (unified-value unified term1)
        term2 (unified-value unified term2))

  (cond
    ((eq* term1 term2)
     unified)
    ((svar-p term1)
     (cons (cons term1 term2) unified))
    ((svar-p term2)
     (cons (cons term2 term1) unified))
    ((and (consp term1) (consp term2))
     (let ((unified (%unify unified (car term1) (car term2))))
       (if (eq unified t)
           t
         (%unify unified (cdr term1) (cdr term2)))))
    (t t)))

(declaim (inline unify))
(defun unify (term1 term2)
  (let ((unified (%unify '() term1 term2)))
    (values (not (eq unified t)) unified)))
