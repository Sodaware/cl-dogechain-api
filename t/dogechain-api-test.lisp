(in-package :dogechain-api-test)

(def-suite dogechain-api)
(in-suite dogechain-api)


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

;; [todo] - Can this be removed?
(defun run-tests ()
  (run! 'dogechain-api))


