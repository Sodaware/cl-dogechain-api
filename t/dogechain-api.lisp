(in-package :cl-user)
(defpackage dogechain-api-test
  (:use :cl
        :dogechain-api
        :prove
        :cl-mock))
(in-package :dogechain-api-test)

(plan 11)


;; ----------------------------------------------------------------------
;; -- Get Address Balance
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
