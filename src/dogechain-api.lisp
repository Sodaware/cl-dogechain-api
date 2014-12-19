(in-package #:dogechain-api)

;; ----------------------------------------------------------------------
;; -- Configuration
;; ----------------------------------------------------------------------

(defparameter +endpoint+ "http://dogechain.info"
  "URI endpoint for Dogechain.info")

(defparameter +simple-endpoint+ "/chain/Dogecoin/q/"
  "Path to the simple endpoint.")


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-address-balance (address)
  "Get amount ever received minus amount ever sent by ADDRESS."
  nil)


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-simple (method &rest params)
  "Get a plaintext result from the chain's METHOD with optional PARAMS."
  (let ((drakma:*header-stream* nil))
    (drakma:http-request (build-simple-endpoint method params))))

(defun build-simple-endpoint (method &optional params)
  "Create the address endpoint for a simple call to METHOD with optional PARAMS."
  (let ((query-string ""))
    (when (not (null params))
      (setf query-string (format nil "~{~a~^/~}" (remove nil params) "/")))
    (format nil "~a~a~a/~a"
            +endpoint+
            +simple-endpoint+
            method
            query-string)))
