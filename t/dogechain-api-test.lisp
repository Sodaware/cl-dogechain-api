(in-package :dogechain-api-test)

(def-suite dogechain-api)
(in-suite dogechain-api)


;; ----------------------------------------------------------------------
;; -- Internal Helper Tests
;; ----------------------------------------------------------------------

(test can-create-simple-endpoint-without-params
      (is (string= "http://dogechain.info/chain/Dogecoin/q/testmethod/"
                   (dogechain-api::build-simple-endpoint "testmethod"))))

;; [todo] - Can this be removed?
(defun run-tests ()
  (run! 'dogechain-api))


