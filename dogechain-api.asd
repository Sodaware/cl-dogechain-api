(in-package :cl-user)
(defpackage dogechain-api-asd
  (:use :cl :asdf))
(in-package :dogechain-api-asd)

(defsystem dogechain-api
  :version "0.1.0"
  :author "Phil Newton"
  :license "GPL 3.0"
  :description "Dogechain.info API library"
  :depends-on (:iterate :drakma :cl-json)
  :pathname "src"
  :components ((:file "package")
               (:file "configuration")
               (:file "dogechain-api"))
  :in-order-to ((test-op (test-op dogechain-api-test))))
