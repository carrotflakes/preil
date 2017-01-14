(in-package :cl-user)
(defpackage preil.prelude
  (:nicknames :prelude)
  (:use :cl :preil)
  (:export #:import-definition
           #:=
           #:member
           #:append
           #:reverse
           #:conc
           #:conc*
           #:and
           #:or
           #:if
           #:eval
           #:print
           #:format
           #:find-1
           #:find-all
           #:not
           #:<
           #:<=
           #:>
           #:>=
           #:inc
           #:numberp
           #:integerp
           #:symbolp))
(in-package :preil.prelude)


(defun sub (term bindings)
  (cond
    ((symbolp term)
     (let ((pair (assoc term bindings)))
       (if pair
           (cdr pair)
           term)))
    ((consp term)
     (let ((car (sub (car term) bindings))
           (cdr (sub (cdr term) bindings)))
       (if (and (eq car (car term))
                (eq cdr (cdr term)))
           term
           (cons car cdr))))
    (t
     term)))

(defun import-definition (&optional (*world* *world*))

  (<- (= ?x ?x))

  (<- (member ?x (?x . ?)))
  (<- (member ?x (? . ?y))
    (member ?x ?y))

  (<- (append () ?xs ?xs))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
    (append ?xs ?ys ?zs))

  (<- (reverse ?xs ?ys)
    (reverse ?xs () ?ys))
  (<- (reverse (?x . ?xs) ?ys ?zs)
    (reverse ?xs (?x . ?ys) ?zs))
  (<- (reverse () ?ys ?ys))

  (<- (conc () ()))
  (<- (conc (() . ?xss) ?ys)
    (conc ?xss ?ys))
  (<- (conc ((?x . ?xs) . ?xss) (?x . ?ys))
    (conc (?xs . ?xss) ?ys))

  (<- (conc* () ()))
  (<- (conc* ((?x) . ?xss) (?x . ?ys))
    (conc* ?xss ?ys))
  (<- (conc* ((?x ?x* . ?xs) . ?xss) (?x . ?ys))
    (conc* ((?x* . ?xs) . ?xss) ?ys))


  (<- (or ?x . ?)
    ?x)
  (<- (or ? . ?xs)
    (or . ?xs))

  (<- (and))
  (<- (and ?x . ?xs)
    ?x
    (and . ?xs))

  (<- (if ?cond ?then ?else)
      (or (and ?cond
               ?then)
          (and (not ?cond)
               ?else)))


  (%- (eval ?result ?form)
    ((?form)
     (satisfy :?result (eval ?form))))

  (%- (format ?out ?form . ?rest)
      ((?form ?rest)
       (satisfy :?out (apply #'format nil ?form ?rest))))

  (%- (print . ?values)
      ((?values)
       (format t "~{~a~^ ~}~%" ?values)
       (satisfy)))

  (%- (not ?term)
    ((?term)
     (unless (solvep ?term)
       (satisfy))))

  (%- (find-1 ?result ?var-syms ?term)
    ((?var-syms ?term)
     (and (listp ?var-syms)
          (every #'symbolp ?var-syms)
          (let* ((variables (mapcar (lambda (symbol)
                                      (gensym (format nil "?~a"
                                                      (symbol-name symbol))))
                                    ?var-syms))
                 (clause (sub ?term (mapcar #'cons ?var-syms variables))))
            (multiple-value-bind (result succeeded)
                (preil:%solve-1 variables (list clause))
              (when succeeded
                (satisfy :?result result)))))))

  ;; e.g.
  ;; (solve-all ?result '(find-all ?result (x y) (append x y (1 2 3))))
  ;; => ((NIL (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) NIL))
  (%- (find-all ?result ?var-syms ?term)
    ((?var-syms ?term)
     (and (listp ?var-syms)
          (every #'symbolp ?var-syms)
          (let* ((variables (mapcar (lambda (symbol)
                                      (gensym (format nil "?~a"
                                                      (symbol-name symbol))))
                                    ?var-syms))
                 (clause (sub ?term (mapcar #'cons ?var-syms variables))))
            (satisfy :?result (preil:%solve-all variables (list clause)))))))

  (%- (inc ?x ?y)
    ((?x)
     (when (integerp ?x)
       (satisfy :?y (1+ ?x))))
    ((?y)
     (when (integerp ?y)
       (satisfy :?x (1- ?y)))))

  (%- (< . ?xs)
    ((?xs)
     (when (and (every #'numberp ?xs) (apply #'< ?xs))
       (satisfy))))
  (%- (<= . ?xs)
    ((?xs)
     (when (and (every #'numberp ?xs) (apply #'<= ?xs))
       (satisfy))))
  (%- (> . ?xs)
    ((?xs)
     (when (and (every #'numberp ?xs) (apply #'> ?xs))
       (satisfy))))
  (%- (>= . ?xs)
    ((?xs)
     (when (and (every #'numberp ?xs) (apply #'>= ?xs))
       (satisfy))))

  (%- (numberp ?value)
      ((?value)
       (when (numberp ?value)
         (satisfy))))

  (%- (integerp ?value)
      ((?value)
       (when (integerp ?value)
         (satisfy))))

  (%- (symbolp ?value)
      ((?value)
       (when (symbolp ?value)
         (satisfy))))
)
