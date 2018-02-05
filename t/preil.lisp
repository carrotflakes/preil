(in-package :cl-user)
(defpackage preil-test
  (:use :cl
        :preil
        :prove))
(in-package :preil-test)

;; NOTE: To run this test file, execute `(asdf:test-system :preil)' in your Lisp.

(plan nil)


(defvar world
  (create-world
    (import-world :prelude)))


(is (solve-all world ?x '(append (1 2) (3 4) ?x))
  '((1 2 3 4))
  :test #'equal)

(is (solve-all world ?x '(format ?x "~a-~b" a 1))
  '("A-1")
  :test #'equal)

(is (solvep world '(< 1 2))
  t
  :test #'equal)

(is (solve-1 world ?x '(prelude:find-1 (?x) (x) (member x (1 2 3))))
  '1
  :test #'equal)

(is (solve-1 world ?x '(prelude:find-all ?x (x y) (append x y (1 2 3))))
  '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
  :test #'equal)


(finalize)
