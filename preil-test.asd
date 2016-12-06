#|
  This file is a part of preil project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage preil-test-asd
  (:use :cl :asdf))
(in-package :preil-test-asd)

(defsystem preil-test
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:preil
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "preil"))))
  :description "Test system for preil"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
