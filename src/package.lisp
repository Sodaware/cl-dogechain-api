(defpackage #:dogechain-api
  (:use #:cl #:drakma :cl-json)
  (:export #:get-address-balance
           #:address-to-hash
           #:valid-address-p
           #:decode-address
           #:get-block-count
           #:get-difficulty
           #:get-received-by-address
           #:get-sent-by-address
           #:hash-to-address
           #:get-total-mined
           #:dogechain-api-error))
