#|
  This file is a part of preil project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage preil-prelude-asd
  (:use :cl :asdf))
(in-package :preil-prelude-asd)

(defsystem preil-prelude
  :version "0.1"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:preil)
  :components ((:module "src"
                :components
                ((:file "prelude"))))
  :in-order-to ((test-op (test-op preil-test))))
