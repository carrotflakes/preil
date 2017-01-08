(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil :preil-prelude)))

(use-package :preil)


(with-world ()

  (prelude:import-definition)

  (<- (hanoi 1 ?from ?to ? ?result)
    (prelude:= ((?from -> ?to)) ?result))
  (<- (hanoi ?n ?from ?to ?via ?result)
    (prelude:eval ?n1 (1- ?n))
    (hanoi ?n1 ?from ?via ?to ?result1)
    (hanoi ?n1 ?via ?to ?from ?result2)
    (prelude:conc (?result1 ((?from -> ?to)) ?result2) ?result))

  (print (solve-1 ?result '(hanoi 3 a b c ?result)))
  )
