;;; json-api.lisp -- Functions for the JSON api.

(in-package #:dogechain-api)

;; ----------------------------------------
;; -- Endpoint Configuration

(defparameter +endpoint+ "http://dogechain.info"
  "URI endpoint for Dogechain.info")

(defparameter +simple-endpoint+ "/chain/Dogecoin/q/"
  "Path to the simple endpoint.")

(defparameter +json-endpoint+ "/api/v1/"
  "Path to the json endpoint.")

(defvar *last-called-url* ""
  "The last URL that was queried.")


