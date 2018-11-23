(in-package :cl-user)
(defpackage preil.unify
  (:use :cl
        :preil.util)
  (:export #:*memory*
           #:*pointer*
           #:*write-table*
           #:*write-table-pointer*
           #:make-memory
           #:make-write-table
           #:memory-rewind
           #:unify
           #:substantiate))
(in-package :preil.unify)


(defvar *memory* nil)
(defvar *pointer* 0)
(defvar *write-table* nil)
(defvar *write-table-pointer* 0)

(defmacro memory-term (pointer)
  `(svref *memory* (* ,pointer 2)))

(defmacro memory-pointer (pointer)
  `(svref *memory* (+ (* ,pointer 2) 1)))

(defun make-memory (size)
  (make-array (* size 2) :element-type 't :initial-element 'no)) ; TODO: no -> 0

(defun make-write-table (size)
  (make-array size :element-type 'fixnum))

(defun memory-rewind (write-table-pointer) ; TODO: compare with FILL
  '(declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (loop
    while (< write-table-pointer *write-table-pointer*)
    do (setf (memory-term (aref *write-table* (decf *write-table-pointer*)))
             'no)))

(declaim (inline svar-term))
(defun svar-term (svar pointer)
  (values (memory-term (+ pointer (svar-local-index svar)))
          (memory-pointer (+ pointer (svar-local-index svar)))))

(declaim (inline svar-bind))
(defun svar-bind (svar pointer term term-pointer)
  (incf pointer (svar-local-index svar))
  (setf (memory-term pointer) term
        (memory-pointer pointer) term-pointer
        (aref *write-table* *write-table-pointer*) pointer)
  (incf *write-table-pointer*))

(declaim (inline unified-value))
(defun unified-value (term pointer)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop
    while (svar-p term)
    do (multiple-value-bind (term* pointer*)
           (svar-term term pointer)
         (if (eq term* 'no)
             (return)
             (setf term term*
                   pointer pointer*))))
  (values term pointer))

(defun unify% (term1 pointer1 term2 pointer2)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))

  (multiple-value-setq (term1 pointer1) (unified-value term1 pointer1))
  (multiple-value-setq (term2 pointer2) (unified-value term2 pointer2))

  (cond
    ((and (eq* term1 term2) (or (not (svar-p term1)) (eq pointer1 pointer2))) ; XXX
     t)
    ((svar-p term1)
     (svar-bind term1 pointer1 term2 pointer2)
     t)
    ((svar-p term2)
     (svar-bind term2 pointer2 term1 pointer1)
     t)
    ((and (consp term1) (consp term2))
     (and (unify% (car term1) pointer1 (car term2) pointer2)
          (unify% (cdr term1) pointer1 (cdr term2) pointer2)))))

(defun unify (term pointer new-term)
  (unify% term pointer new-term *pointer*))

(defun substantiate (term pointer)
  (cond
    ((svar-p term)
     (multiple-value-bind (term pointer)
         (unified-value term pointer)
       (if (svar-p term)
           term
           (substantiate term pointer))))
    ((consp term)
     (cons (substantiate (car term) pointer)
           (substantiate (cdr term) pointer)))
    (t
     term)))
