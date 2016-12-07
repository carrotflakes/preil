(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :preil))

(use-package :preil)

(with-world ()
  (-- (人間 ソクラテス))
  (-- (人間 アリストテレス))
  (-- (死ぬ _x)
      (人間 _x))

  (?print _x
          (死ぬ _x)))

(with-world ()
  (-- (member _x (_x . _)))
  (-- (member _x (_ . _y))
      (member _x _y))

  (print (? (member 1 (1 2 3))))
  (print (? (member 3 (1 2 3))))
  (print (? (member 4 (1 2 3))))
  (print (?1 _x
             (member a _x)))
  (print (?1 _x
             (member a _x)
             (member b _x)
             (member c _x)))

  (-- (append () _xs _xs))
  (-- (append (_x . _xs) _ys (_x . _zs))
      (append _xs _ys _zs))

  (print (?all _x
               (append (1 2 3) (a b c) _x)))
  )
