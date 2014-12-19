(in-package :dogechain-api-test)

(def-suite dogechain-api)
(in-suite dogechain-api)


;; ----------------------------------------------------------------------
;; -- Simple Method Tests
;; ----------------------------------------------------------------------



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
