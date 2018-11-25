(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(initialize-memory 1000)

(in-world (make-world))
(<- (人間 ソクラテス))
(<- (人間 アリストテレス))
(<- (死ぬ ?x)
    (人間 ?x))
(do-solve
    ((?x) (print ?x))
  '(死ぬ ?x))


(in-world (make-world))

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

(<- (= ?x ?x))


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


(print (solvep '(member 1 (1 2 3))))
(print (solvep '(member 3 (1 2 3))))
(print (solvep '(member 4 (1 2 3))))
(print (solve-1 ?x
                '(member a ?x)))
(print (solve-1 ?x
                '(member a ?x)
                '(member b ?x)
                '(member c ?x)))


(print (solve-all ?x
                  '(append (1 2 3) (a b c) ?x)))


(print (solve-all ?z
                  '(= ?x (1 2 3))
                  '(reverse ?x ?y)
                  '(append ?x ?y ?z)))

(print (solve-all (?x ?y)
                  '(append ?x ?y (1 2 3 4))))


(print (solve-all ?x
                  '(conc ((1 2) (3) (4 5 6)) ?x)))
(print (solve-all (?x ?y ?z)
                  '(conc (?x ?y ?z) (1 2 3))))

(print (solve-1 ?x
                '(= ?x (? ? ? ? ?))
                '(member 1 ?x)
                '(member 2 ?x)
                '(member 3 ?x)
                '(reverse ?x ?x)))


(print (solve-all ?x
                  '(conc* ((1 2) (3) (4 5 6)) ?x)))
(print (solve-all (?x ?y ?z)
                  '(conc* (?x ?y ?z) (1 2 3))))
(print (solve-all (?x ?y ?z)
                  '(conc* (?x ?y ?z) (1 2 3 4))))
