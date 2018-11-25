(in-package :cl-user)
(defpackage preil.dictionary
  (:use :cl)
  (:export #:dictionary
           #:make-dictionary
           #:add
           #:merge-dictionary
           #:consult))
(in-package :preil.dictionary)

(defstruct dictionary
  (table (make-hash-table :test 'eq))
  unindexable)

(defun indexable-p (term)
  (and (consp term)
       (symbolp (car term))))

(defun index (term)
  (car term))

(defun add (head item dictionary)
  (with-slots (table unindexable) dictionary
    (if (indexable-p head)
        (setf (gethash (index head) table)
              (append (gethash (index head) table) (list item)))
        (push item unindexable))))

(defun merge-dictionary (dictionary-1 dictionary-2)
  (maphash (lambda (key value)
             (setf (gethash key (dictionary-table dictionary-1))
                   (append (gethash key (dictionary-table dictionary-1))
                           (gethash key (dictionary-table dictionary-2)))))
           (dictionary-table dictionary-2))
  (setf (dictionary-unindexable dictionary-1)
        (append (dictionary-unindexable dictionary-1)
                (dictionary-unindexable dictionary-2))))

(defun consult (head dictionary)
  (declare (inline indexable-p index))
  (with-slots (table unindexable) dictionary
    (if (indexable-p head)
        (gethash (index head) table)
        unindexable)))
