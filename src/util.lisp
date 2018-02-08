(in-package :cl-user)
(defpackage preil.util
  (:use :cl)
  (:export #:*variable-prefix*
           #:*bound-id*
           #:make-svar
           #:svar-name
           #:svar-bound-id
           #:svar-p
           #:eq*
           #:variablep
           #:contains-svar-p
           #:collect-variables
           #:bind-new-svar
           #:sub
           #:ssub
           #:variables-cleanse-bindings
           #:cleanse-term
           #:unify
           #:reset-bound-id
           #:next-bound-id))
(in-package :preil.util)


(declaim (inline *variable-prefix*))
(defvar *variable-prefix* "?")


(defvar *bound-id* -1)


(defstruct svar
  name
  (bound-id -1)
  (value nil))

'(defmethod print-object ((obj svar) out)
  #+sbcl(format out "~a~a" (svar-name obj)
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

(defun variables-cleanse-bindings (variables)
  (loop
     for variable in variables
     when (string/= variable *variable-prefix*)
     collect (cons variable (make-svar :name (symbol-name variable)))))

(defun cleanse-term (term)
  (sub term
       (variables-cleanse-bindings (collect-variables term))))


(declaim (inline unified-value))
(defun unified-value (term)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop
     while (and (svar-p term)
                (= (svar-bound-id term) *bound-id*))
     do (setf term (svar-value term)))
  term)

(defun sub (term bindings)
  (cond
    ((variablep term)
     (if (string= term *variable-prefix*)
         (make-svar :name *variable-prefix*)
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

(defun bind-new-svar (term)
  (cond
    ((consp term)
     (bind-new-svar (car term))
     (bind-new-svar (cdr term)))
    ((and (svar-p term)
          (/= (svar-bound-id term) *bound-id*))
     (setf (svar-bound-id term) *bound-id*
           (svar-value term) (make-svar :name (svar-name term))))))

(defun ssub (term)
  (setf term (unified-value term))
  (if (consp term)
      (let ((car (ssub (car term)))
            (cdr (ssub (cdr term))))
        (if (and (eq* car (car term))
                 (eq* cdr (cdr term)))
            term
            (cons car cdr)))
      term))

(defun unify (term1 term2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  '(format t "~a ~a~%" term1 term2)

  (setf term1 (unified-value term1)
        term2 (unified-value term2))

  (cond
    ((eq* term1 term2)
     t)
    ((svar-p term1)
     (setf (svar-bound-id term1) *bound-id*
           (svar-value term1) term2)
     t)
    ((svar-p term2)
     (setf (svar-bound-id term2) *bound-id*
           (svar-value term2) term1)
     t)
    ((and (consp term1) (consp term2))
     (and (unify (car term1) (car term2))
          (unify (cdr term1) (cdr term2))))))


(defun reset-bound-id ()
  (setf *bound-id* -1))

(defun next-bound-id ()
  (incf *bound-id*))
