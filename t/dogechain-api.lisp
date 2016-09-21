(in-package :cl-user)
(defpackage dogechain-api-test
  (:use :cl
        :dogechain-api
        :prove
        :cl-mock))
(in-package :dogechain-api-test)

(plan 11)


;; ----------------------------------------------------------------------
;; -- get-address-balance
;; ----------------------------------------------------------------------

(subtest ":get-address-balance"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/addressbalance/invalid_address"
    "Error: address invalid"
    (is-error (dogechain-api:get-address-balance "invalid_address") 'invalid-address-error
              "Raises a 'invalid-address-error for invalid addresses."))
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/addressbalance/valid_address"
    "1.2345"
    (let ((balance (dogechain-api:get-address-balance "valid_address")))
      (is-type balance 'float "Returns a floating point balance")
      (is balance 1.2345 "Returns the correct balance"))))


;; ----------------------------------------------------------------------
;; -- address-to-hash
;; ----------------------------------------------------------------------

(subtest ":address-to-hash"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/addresstohash/invalid_address"
    "Error: address invalid"
    (is-error (dogechain-api:address-to-hash "invalid_address") 'invalid-address-error
              "Raises a `invalid-address-error for invalid addresses."))
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/addresstohash/valid_address"
    "hashed-result"
    (is (dogechain-api:address-to-hash "valid_address") "hashed-result"
        "Returns result as a string.")))


;; ----------------------------------------------------------------------
;; -- valid-address-p
;; ----------------------------------------------------------------------

(subtest ":valid-address-p"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/checkaddress/invalid_address"
    "Error: address invalid"
    (let ((response (dogechain-api:valid-address-p "invalid_address"))) 
      (is-type response 'boolean "Returns boolean result for invalid address.")
      (isnt response t "Returns nil for invalid address")))
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/checkaddress/valid_address"
    "1E"
    (let ((response (dogechain-api:valid-address-p "valid_address"))) 
      (is-type response 'boolean "Returns boolean result for valid address.")
      (is response t "Returns t for valid address"))))


;; ----------------------------------------------------------------------
;; -- decode-address
;; ----------------------------------------------------------------------

(subtest ":decode-address"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/decode_address/invalid_address"
    "Error: address invalid"
    (is-error (dogechain-api:decode-address "invalid_address") 'invalid-address-error
              "Raises 'invalid-address-error for invalid addresses.")) 
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/decode_address/valid_address"
    "1E:hash_goes_here"
    (let ((response (dogechain-api:decode-address "valid_address")))
      (is (cdr (assoc :version response)) "1E" "Returns address version for valid address")
      (is (cdr (assoc :hash response)) "hash_goes_here" "Returns address hash for valid address"))))


;; ----------------------------------------------------------------------
;; -- get-received-by-address
;; ----------------------------------------------------------------------

(subtest ":get-received-by-address"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/getreceivedbyaddress/invalid_address"
    "Error: address invalid"
    (is-error (dogechain-api:get-received-by-address "invalid_address") 'invalid-address-error
              "Raises 'invalid-address-error for invalid addresses."))
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/getreceivedbyaddress/valid_address"
    "1.2345"
    (let ((received-amount (dogechain-api:get-received-by-address "valid_address")))
      (is-type received-amount 'float "Returns a floating point amount")
      (is received-amount 1.2345 "Returns the amount ever received"))))


;; ----------------------------------------------------------------------
;; -- get-sent-by-address
;; ----------------------------------------------------------------------

(subtest ":get-sent-by-address"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/getsentbyaddress/invalid_address"
    "Error: address invalid"
    (is-error (dogechain-api:get-sent-by-address "invalid_address") 'invalid-address-error
              "Raises 'invalid-address-error for invalid addresses."))
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/getsentbyaddress/valid_address"
    "1.2345"
    (let ((sent-amount (dogechain-api:get-sent-by-address "valid_address")))
      (is-type sent-amount 'float "Returns a floating point amount")
      (is sent-amount 1.2345 "Returns the amount ever sent"))))


;; ----------------------------------------------------------------------
;; -- hash-to-address
;; ----------------------------------------------------------------------

(subtest ":hash-to-address"
  (is-error (dogechain-api:hash-to-address "hash") 'dogechain-api-error
            "Raises error when called"))


;; ----------------------------------------------------------------------
;; -- get-total-mined
;; ----------------------------------------------------------------------

(subtest ":get-total-mined"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/totalbc/"
    "12345.6789"
    (let ((mined-amount (dogechain-api:get-total-mined)))
      (is-type mined-amount 'float "Returns a floating point amount")
      (is mined-amount 12345.6789 "Returns the total number of coins ever mined"))))


;; ----------------------------------------------------------------------
;; -- Internal Helper Tests
;; ----------------------------------------------------------------------

(subtest "::build-simple-endpoint"
  (is (dogechain-api::build-simple-endpoint "testmethod")
      "http://dogechain.info/chain/Dogecoin/q/testmethod/"
      "Builds endpoint without parameters")
  (is (dogechain-api::build-simple-endpoint "TeStMeThOd")
      "http://dogechain.info/chain/Dogecoin/q/testmethod/"
      "Converts actions to lowercase") 
  (is (dogechain-api::build-simple-endpoint "testmethod" '("param-1"))
      "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
      "Appends single parameter to endpoint")
  (is (dogechain-api::build-simple-endpoint "testmethod" '("param-1" "param-2"))
      "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
      "Appends multiple parameters to endpoint")
  (is (dogechain-api::build-simple-endpoint "testmethod" '(nil nil nil))
      "http://dogechain.info/chain/Dogecoin/q/testmethod/"
      "Does not append nil parameters to endpoint"))

(subtest "::get-simple"
  (with-mocked-request "http://dogechain.info/chain/Dogecoin/q/testmethod/"
    "test<!-- comments for security -->"
    (is (dogechain-api::get-simple "testmethod") "test"
        "Removes HTML comments from end of responses")))

(subtest "::strip-html-comments"
  (is (dogechain-api::strip-html-comments "12345<!-- comment -->")
      "12345"
      "Removes HTML comments from string when present")

  (is (dogechain-api::strip-html-comments "12345")
      "12345"
      "Does not modify string when no HTML comments present"))

(finalize)
