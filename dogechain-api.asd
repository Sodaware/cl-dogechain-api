(asdf:defsystem #:dogechain-api
    :version "0.1.0"
    :serial t
    :license "GPL 3.0"
    :description "Dogechain.info API library"
    :author "Phil Newton"
    :depends-on (:iterate :drakma :cl-json)
    :pathname "src"
    :components ((:file "package")
                 (:file "dogechain-api")))
