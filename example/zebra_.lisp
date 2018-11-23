;;;; http://franz.com/support/documentation/current/doc/prolog.html
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil :preil-prelude)))

(use-package '(:preil :prelude))

(initialize-memory 10000)

(let ((world (create-world

               (import-world :prelude)

  (<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
  (<- (nextto ?x ?y ?list) (iright ?y ?x ?list))
  (<- (iright ?left ?right (?left ?right . ?rest)))
  (<- (iright ?left ?right (?x . ?rest))
      (iright ?left ?right ?rest))
  (<- (zebra ?h ?w ?z)
      ;; Each house is of the form:
      ;; (house nationality pet cigarette drink house-color)
      (= ?h ((house norwegian ? ? ? ?)   ;1,10
             ?
             (house ? ? ? milk ?) ? ?))  ; 9
      (member (house englishman ? ? ? red) ?h) ; 2
      (member (house spaniard dog ? ? ?) ?h) ; 3
      (member (house ? ? ? coffee green) ?h) ; 4
      (member (house ukrainian ? ? tea ?) ?h) ; 5
      (iright (house ? ? ? ? ivory)      ; 6
              (house ? ? ? ? green) ?h)
      (member (house ? snails winston ? ?) ?h) ; 7
      (member (house ? ? kools ? yellow) ?h) ; 8
      (nextto (house ? ? chesterfield ? ?) ;11
              (house ? fox ? ? ?) ?h)
      (nextto (house ? ? kools ? ?)      ;12
              (house ? horse ? ? ?) ?h)
      (member (house ? ? luckystrike oj ?) ?h) ;13
      (member (house japanese ? parliaments ? ?) ?h) ;14
      (nextto (house norwegian ? ? ? ?)  ;15
              (house ? ? ? ? blue) ?h)
      (member (house ?w ? ? water ?) ?h) ;Q1
      (member (house ?z zebra ? ? ?) ?h) ;Q2
      )

  ;; The result: [house(norwegian,fox,kools,water,yellow),house(ukrainian,horse,chesterfield,tea,blue),house(englishman,snails,winston,milk,red),house(spaniard,dog,luckystrike,oj,ivory),house(japanese,zebra,parliaments,coffee,green)]
               )))

  (print (solvep world '(zebra ? ? ?)))
  (print (solve-all world (?houses ?water-drinker ?zebra-owner)
           `(zebra ?houses ?water-drinker ?zebra-owner)))

  (defun zebra-benchmark (&optional (n 1000))
    (declare (optimize (speed 3) (safety 0)))
    (let (rt0 rt1)
      (time (loop initially (setf rt0 (get-internal-run-time))
               repeat n
               do (solvep world '(zebra ?houses ?water-drinker ?zebra-owner))
                                        ; Stop once answer is found.
                                        ; This appears to be
                                        ; what other implementations do,
                                        ; e.g. time/1 in
                                        ; SWI Prolog.
               finally (setf rt1 (get-internal-run-time))))
      (destructuring-bind (zebra-owner water-drinker houses)
          (solve-1 world (?houses ?water-drinker ?zebra-owner)
                   '(zebra ?houses ?water-drinker ?zebra-owner))
        (values (/ (* n 12825) (/ (- rt1 rt0) 1000.0)) ; real time
                                        ; is milliseconds
                zebra-owner water-drinker houses))))

  '(progn
    (sb-profile:profile "PREIL.CORE" "PREIL.UTIL")
    (solve-all world (?houses ?water-drinker ?zebra-owner) `(zebra ?houses ?water-drinker ?zebra-owner))
    (solve-all world (?houses ?water-drinker ?zebra-owner) `(zebra ?houses ?water-drinker ?zebra-owner))
   (print (solve-all world (?houses ?water-drinker ?zebra-owner) `(zebra ?houses ?water-drinker ?zebra-owner)))
   (sb-profile:report))

  (zebra-benchmark 1000)
  ;(print preil.util::*bound-id*)
  )
