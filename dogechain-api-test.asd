(asdf:defsystem #:dogechain-api-test
    :serial t
    :depends-on (#:dogechain-api #:fiveam #:cl-mock)
    :pathname "t/"
    :components ((:file "package")
                 (:file "dogechain-api-test")))
