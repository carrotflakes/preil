(in-package :cl-user)
(defpackage preil.core
  (:use :cl
        :preil.util
        :preil.unify
        :preil.dictionary)
  (:export #:*world*
           #:%make-world
           #:copy-world
           #:world-p
           #:add-clause
           #:add-predicate
           #:merge-world
           #:solve
           #:satisfy
           #:exec
           #:solve))
(in-package :preil.core)


(defvar *world* nil)
(defvar *resolved-function* nil)


(defstruct (world (:constructor %make-world))
  (clauses nil)
  (predicates nil)
  (dictionary (make-dictionary)))

(defstruct clause
  head
  body
  variable-num)

(defstruct predicate
  head
  patterns ; A list of (bound-svars free-svars free-variables function)
  variable-num)


(defun add-clause (world head body)
  (let ((clause (multiple-value-bind (term variable-num)
                    (term-replace-variable-with-svar (cons head body))
                  (make-clause :head (car term)
                               :body (cdr term)
                               :variable-num variable-num))))
    (setf (world-clauses world)
          (append (world-clauses world) (list clause)))
    (add (clause-head clause) clause (world-dictionary world))))

(defun add-predicate (world head patterns)
  (multiple-value-bind (head variable-num bindings)
      (term-replace-variable-with-svar head)
    (let* ((head-variables (mapcar #'car bindings))
           (built-patterns
             (mapcar
              #'(lambda (pattern)
                  (destructuring-bind (bound-variables . body) pattern
                    (let* ((free-variables (set-difference head-variables bound-variables))
                           (satisfy-macro-definition
                             `(satisfy (&key ,@free-variables)
                                       (list 'funcall '%satisfy
                                             (list 'list ,@free-variables)))))
                      (list
                       (sub bound-variables bindings)
                       (sub free-variables bindings)
                       free-variables
                       (eval `(lambda (%satisfy ,@bound-variables)
                                (declare (ignorable %satisfy ,@bound-variables)) ; FIXME
                                (macrolet (,satisfy-macro-definition)
                                  ,@body)))))))
              patterns))
           (predicate (make-predicate :head head
                                      :patterns built-patterns
                                      :variable-num variable-num)))
      (setf (world-predicates world)
            (append (world-predicates world)
                    (list predicate)))
      (add (predicate-head predicate) predicate (world-dictionary world)))))

(defun merge-world (world inner-world) ; FIXME
  (setf (world-clauses world)
        (append (world-clauses world)
                (world-clauses inner-world))
        (world-predicates world)
        (append (world-predicates world)
                (world-predicates inner-world)))
  (merge-dictionary (world-dictionary world) (world-dictionary inner-world)))


(declaim (inline terms-goals))
(defun terms-goals (goals pointer)
  (mapcar (lambda (x) (cons x pointer)) goals))

(declaim (inline dispatch-predicate))
(defun dispatch-predicate (goal goals predicate)
  (let ((write-table-pointer-1 *write-table-pointer*))
    (when (unify (car goal) (cdr goal) (predicate-head predicate))
      (loop
        with write-table-pointer-2 = *write-table-pointer*
        with pointer = *pointer*
        with *pointer* = (+ *pointer* (predicate-variable-num predicate))
        for (bound-svars free-svars free-variables function)
        in (predicate-patterns predicate)
        for parameters = (substantiate bound-svars pointer)
        when (groundp parameters) ; If parameters contain svar, cannot apply the predicate.
        do (apply function
                  (lambda (term) ; term contains no svar.
                    (when (let ((*pointer* pointer)) (unify free-svars *pointer* term))
                      (exec goals))
                    (memory-rewind write-table-pointer-2))
                  parameters)
           (return)
        finally (error "Out of implements for ~a" (substantiate (car goal) (cdr goal)))))
    (memory-rewind write-table-pointer-1)))

(declaim (inline dispatch-clause))
(defun dispatch-clause (goal goals clause)
  (let ((write-table-pointer *write-table-pointer*))
    (when (unify (car goal) (cdr goal) (clause-head clause))
      '(format t "unified~%~a~%~a~%~a~%"
              (car goal)
              (clause-head clause) (subseq *memory* 0 20))
      '(print (list :unified clause))
      (let ((goals (nconc (terms-goals (clause-body clause) *pointer*) goals))
            (*pointer* (+ *pointer* (clause-variable-num clause))))
        (exec goals)))
    (memory-rewind write-table-pointer)))

(defun exec (goals)
  '(print goals)
  '(print (mapcar (lambda (x) (substantiate (car x) (cdr x))) goals))
  '(print (subseq *memory* 0 40))
  '(print (subseq *write-table* 0 40))
  (when (null (cdr goals))
    (when *resolved-function*
      (funcall *resolved-function* (substantiate (caar goals) (cdar goals)))))

  (let ((goal (pop goals)))
    (if (or (svar-p (car goal)) (svar-p (caar goal))) ; FIXME
        (progn
          (dolist (predicate (world-predicates *world*))
            (dispatch-predicate goal goals predicate))
          (dolist (clause (world-clauses *world*))
            (dispatch-clause goal goals clause)))
        (dolist (item (consult (car goal) (world-dictionary *world*)))
          (if (clause-p item)
              (dispatch-clause goal goals item)
              (dispatch-predicate goal goals item))))))

(defun solve (term goals *resolved-function*
              &key (*memory* *memory*) (*pointer* *pointer*))
  '(print 'solve)
  '(print *pointer*)
  '(print *write-table-pointer*)

  (multiple-value-bind (terms variable-num)
      (term-replace-variable-with-svar (append goals (list term)))
    (let ((write-table-pointer *write-table-pointer*)
          (goals (terms-goals terms *pointer*))
          (*pointer* (+ *pointer* variable-num)))
      (exec goals)
      (memory-rewind write-table-pointer)))
  nil)
