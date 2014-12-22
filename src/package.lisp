(defpackage #:dogechain-api
  (:use #:cl #:drakma :cl-json)
  (:export #:get-address-balance
           #:address-to-hash
           #:valid-address?
           #:decode-address
           #:get-block-count
           #:get-difficulty
           #:get-received-by-address
           #:get-sent-by-address))
