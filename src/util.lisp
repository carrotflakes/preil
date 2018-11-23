(in-package :cl-user)
(defpackage preil.util
  (:use :cl)
  (:export #:*variable-prefix*
           #:make-svar
           #:svar-name
           #:svar-local-index
           #:svar-p
           #:eq*
           #:variablep
           #:term-replace-variable-with-svar
           #:sub
           #:groundp))
(in-package :preil.util)


(defvar *variable-prefix* "?")


(defstruct svar
  name
  local-index)

(defmethod print-object ((obj svar) out)
  #+sbcl(format out "~a~a" (svar-name obj) (svar-local-index obj))
  #-sbcl(format out "#~a" (svar-name obj)))


(declaim (inline eq*))
(defun eq* (l r)
	(if (stringp l)
			(and (stringp r) (string= l r))
      (eq l r)))

(defun variablep (object)
  (and (symbolp object)
       (string= object *variable-prefix* :end1 1 :end2 1)))

(defun term-replace-variable-with-svar (term)
  (let ((local-index -1)
        (bindings '()))
    (labels ((f (term)
               (cond
                 ((variablep term)
                  (if (string= term *variable-prefix*)
                      (make-svar :name *variable-prefix*
                                 :local-index (incf local-index))
                      (cdr (or (assoc term bindings)
                               (first (push (cons term
                                                  (make-svar :name (symbol-name term)
                                                             :local-index (incf local-index)))
                                            bindings))))))
                 ((consp term)
                  (cons (f (car term))
                        (f (cdr term))))
                 (t
                  term))))
      (values (f term) (1+ local-index) bindings))))

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

(defun groundp (term)
  (labels ((f (term)
             (cond
               ((svar-p term)
                nil)
               ((consp term)
                (and (f (car term))
                     (f (cdr term))))
               (t
                t))))
    (f term)))
