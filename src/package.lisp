(defpackage #:dogechain-api
  (:use #:cl #:drakma :cl-json)
  (:export

   ;; Error conditions
   #:dogechain-api-error
   #:invalid-address-error

   ;; Main API methods 
   #:address-to-hash
   #:decode-address
   #:get-address-balance
   #:get-block-count
   #:get-difficulty
   #:get-received-by-address
   #:get-sent-by-address
   #:get-total-mined
   #:hash-to-address
   #:valid-address-p))
