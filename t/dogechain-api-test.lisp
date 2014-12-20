(in-package :dogechain-api-test)

(def-suite dogechain-api)
(in-suite dogechain-api)


;; ----------------------------------------------------------------------
;; -- Simple Method Tests
;; ----------------------------------------------------------------------

(test get-address-balance/signals-an-error-when-api-returns-error
      (cl-mock:dflet
       ((drakma:http-request (uri)
                             (is (string= uri "http://dogechain.info/chain/Dogecoin/q/addressbalance/invalid_address"))
                             "Error: invalid address"))
       (signals
        (error "Did not signal error")
        (dogechain-api:get-address-balance "invalid_address"))))

(test get-address-hash/can-get-address-hash
      (cl-mock:dflet
       ((drakma:http-request (uri)
                             (is (string= uri "http://dogechain.info/chain/Dogecoin/q/addresstohash/TEST_ADDRESS"))
                             "SOME_HASH"))
       (dogechain-api:address-to-hash "TEST_ADDRESS")))


;; ----------------------------------------------------------------------
;; -- Internal Helper Tests
;; ----------------------------------------------------------------------

(test can-create-simple-endpoint-without-params
      (is (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api::build-simple-endpoint "testmethod"))))

(test can-create-simple-endpoint-with-params
      (is (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1"
                   (dogechain-api::build-simple-endpoint "testmethod" '("param-1"))))
      (is (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/param-1/param-2"
                   (dogechain-api::build-simple-endpoint "testmethod" '("param-1" "param-2")))))

(test create-simple-endpoint-does-not-add-empty-params
      (is (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api::build-simple-endpoint "testmethod" '(nil nil nil)))))

(test can-get-simple-response
      (cl-mock:dflet
       ((drakma::http-request (uri)
                              (is (string= uri "http://dogechain.info/chain/Dogecoin/q/testmethod/"))
                              "test"))
       (is (string= "test" (dogechain-api::get-simple "testmethod")))))
