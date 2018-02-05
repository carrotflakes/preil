(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)


(do-solve
  (create-world
    (<- (人間 ソクラテス))
    (<- (人間 アリストテレス))
    (<- (死ぬ ?x)
      (人間 ?x)))

  ((?x) (print ?x))
  '(死ぬ ?x))

(let ((world
       (create-world
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
         )))

  (print (solvep world '(member 1 (1 2 3))))
  (print (solvep world '(member 3 (1 2 3))))
  (print (solvep world '(member 4 (1 2 3))))
  (print (solve-1 world ?x
                  '(member a ?x)))
  (print (solve-1 world ?x
                  '(member a ?x)
                  '(member b ?x)
                  '(member c ?x)))


  (print (solve-all world ?x
                    '(append (1 2 3) (a b c) ?x)))


  (print (solve-all world ?z
                    '(= ?x (1 2 3))
                    '(reverse ?x ?y)
                    '(append ?x ?y ?z)))

  (print (solve-all world (?x ?y)
                    '(append ?x ?y (1 2 3 4))))


  (print (solve-all world ?x
                    '(conc ((1 2) (3) (4 5 6)) ?x)))
  (print (solve-all world (?x ?y ?z)
                    '(conc (?x ?y ?z) (1 2 3))))

  (print (solve-1 world ?x
                  '(= ?x (? ? ? ? ?))
                  '(member 1 ?x)
                  '(member 2 ?x)
                  '(member 3 ?x)
                  '(reverse ?x ?x)))


  (print (solve-all world ?x
                    '(conc* ((1 2) (3) (4 5 6)) ?x)))
  (print (solve-all world (?x ?y ?z)
                    '(conc* (?x ?y ?z) (1 2 3))))
  (print (solve-all world (?x ?y ?z)
                    '(conc* (?x ?y ?z) (1 2 3 4))))
  )
