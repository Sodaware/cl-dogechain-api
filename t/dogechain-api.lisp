(in-package :cl-user)
(defpackage dogechain-api-test
  (:use :cl
        :dogechain-api
        :prove
        :cl-mock))
(in-package :dogechain-api-test)

(plan 31)


;; ----------------------------------------------------------------------
;; -- get-address-balance
;; ----------------------------------------------------------------------

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/addressbalance/invalid_address")
      "Error: address invalid"))
  (is-error (dogechain-api:get-address-balance "invalid_address") 'api-error))

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/addressbalance/valid_address")
      "1.2345"))
  (let ((balance (dogechain-api:get-address-balance "valid_address")))
    (is balance 1.2345)
    (is-type balance 'float)))


;; ----------------------------------------------------------------------
;; -- address-to-hash
;; ----------------------------------------------------------------------

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/addresstohash/invalid_address")
      "Error: address invalid"))
  (is-error (dogechain-api:address-to-hash "invalid_address") 'api-error))

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/addresstohash/valid_address")
      "hashgoeshere"))
  (is (dogechain-api:address-to-hash "valid_address") "hashgoeshere"))


;; ----------------------------------------------------------------------
;; -- valid-address-p
;; ----------------------------------------------------------------------

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/checkaddress/invalid_address")
      "Error: address invalid"))
  (isnt (dogechain-api:valid-address-p "invalid_address") T))

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/checkaddress/valid_address")
      "1E"))
  (let ((response (dogechain-api:valid-address-p "valid_address")))
    (ok response)
    (is-type response 'boolean)))


;; ----------------------------------------------------------------------
;; -- decode-address
;; ----------------------------------------------------------------------

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/decode_address/invalid_address")
      "Error: address invalid"))
  (is-error (dogechain-api:decode-address "invalid_address") 'api-error))

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/decode_address/valid_address")
      "1E:hash_goes_here"))
  (let ((response (dogechain-api:decode-address "valid_address")))
    (is (cdr (assoc :version response)) "1E")
    (is (cdr (assoc :hash response)) "hash_goes_here")))


;; ----------------------------------------------------------------------
;; -- get-received-by-address
;; ----------------------------------------------------------------------

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/getreceivedbyaddress/invalid_address")
      "Error: address invalid"))
  (is-error (dogechain-api:get-received-by-address "invalid_address") 'api-error))

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/getreceivedbyaddress/valid_address")
      "1.2345"))
  (let ((received-amount (dogechain-api:get-received-by-address "valid_address")))
    (is received-amount 1.2345)
    (is-type received-amount 'float)))


;; ----------------------------------------------------------------------
;; -- hash-to-address
;; ----------------------------------------------------------------------

(is-error (dogechain-api:hash-to-address "hash") 'api-error)


;; ----------------------------------------------------------------------
;; -- Internal Helper Tests
;; ----------------------------------------------------------------------

(is (dogechain-api::build-simple-endpoint "testmethod")
    "http://dogechain.info/chain/Dogecoin/q/testmethod/")

(is (dogechain-api::build-simple-endpoint "testmethod" '("param-1"))
    "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1")

(is (dogechain-api::build-simple-endpoint "testmethod" '("param-1" "param-2"))
    "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2")

(is (dogechain-api::build-simple-endpoint "testmethod" '(nil nil nil))
    "http://dogechain.info/chain/Dogecoin/q/testmethod/")

(with-mocks ()
  (answer (drakma:http-request uri)
    (progn
      (is uri "http://dogechain.info/chain/Dogecoin/q/testmethod/")
      "test"))
  (is (dogechain-api::get-simple "testmethod") "test"))

(finalize)
