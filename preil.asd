#|
  This file is a part of preil project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage preil-asd
  (:use :cl :asdf))
(in-package :preil-asd)

(defsystem preil
  :version "0.1"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "util")
                 (:file "dictionary")
                 (:file "unify" :depends-on ("util"))
                 (:file "core" :depends-on ("util" "unify" "dictionary"))
                 (:file "preil" :depends-on ("util" "unify" "core")))))
  :description "A logic programming library for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op preil-test))))
