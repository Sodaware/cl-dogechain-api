(defpackage #:dogechain-api
  (:use #:cl #:drakma :cl-json)
  (:export #:get-address-balance
           #:address-to-hash
           #:valid-address?
           #:decode-address))
