;;; t/test-util.lisp -- Utility functions for testing.

(in-package :cl-user)
(defpackage dogechain-api-test
  (:use :cl
        :dogechain-api
        :prove
        :cl-mock))
(in-package :dogechain-api-test)


;; ----------------------------------------
;; -- Mock Helpers

(defmacro with-mocked-request (uri response &body body)  
  `(with-mocks ()
     (answer (drakma:http-request uri)
             ,response)
     (progn ,@body)))
