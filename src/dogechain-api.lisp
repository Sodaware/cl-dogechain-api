;;; dogechain-api.lisp -- Simple API functionality.

(in-package #:dogechain-api)


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-address-balance (address)
  "Get amount ever received minus amount ever sent by ADDRESS."
  (let ((response (get-simple "addressbalance" address)))
    (if (string= response "Error: address invalid")
        (dogechain-api-error "Address invalid")
        (read-from-string response))))

(defun address-to-hash (address)
  "Get the public key hash for ADDRESS."
  (let ((response (get-simple "addresstohash" address)))
    (if (string= response "Error: address invalid")
        (dogechain-api-error "Address invalid")
        response)))

(defun valid-address-p (address)
  "Check ADDRESS for validity."
  (string= "1E" (get-simple "checkaddress" address)))

(defun decode-address (address)
  "Get the version prefix and hash encoded in ADDRESS."
  (let* ((response (get-simple "decode_address" address))
         (parts (cl-ppcre:split ":" response)))
    (if (string= response "Error: address invalid")
        (dogechain-api-error "Address invalid")
        `((:version . ,(car parts))
          (:hash . ,(cadr parts))))))

(defun get-block-count ()
  "Get the current block number."
  (read-from-string (get-simple "getblockcount")))

(defun get-difficulty ()
  "Get the difficulty of the last solved block."
  (read-from-string (get-simple "getdifficulty")))

(defun get-address-received (address)
  "Get the total amount of Dogecoin ever received by ADDRESS."
  (get-received-by-address address))

(defun get-address-sent (address)
  "Get the total amount of Dogecoin ever sent by ADDRESS."
  (get-sent-by-address address))

(defun get-total-mined ()
  "Get the total amount of currency ever mined."
  (read-from-string (get-simple "totalbc")))

(defun get-received-by-address (address)
  "Get the total amount of Dogecoin ever received by ADDRESS."
  (let ((response (get-simple "getreceivedbyaddress" address)))
    (if (string= response "Error: address invalid")
        (dogechain-api-error "Address invalid")
        (read-from-string response))))

(defun get-sent-by-address (address)
  "Get the total amount of Dogecoin ever sent by ADDRESS."
  (let ((response (get-simple "getsentbyaddress" address)))
    (if (string= response "Error: address invalid")
        (dogechain-api-error "Address invalid")
        (read-from-string response))))

(defun hash-to-address (hash)
  "REMOVED FROM REMOTE API"
  (dogechain-api-error "hash-to-address has been removed"))


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-simple (method &rest params)
  "Get a plaintext result from the chain's METHOD with optional PARAMS."
  (let ((drakma:*header-stream* nil)
        (url (build-simple-endpoint method params)))
    (setf *last-called-url* url)
    (strip-html-comments (drakma:http-request url))))

(defun build-simple-endpoint (method &optional params)
  "Create the address endpoint for a simple call to METHOD with optional PARAMS."
  (let ((query-string ""))
    (when (not (null params))
      (setf query-string (format nil "~{~a~^/~}" (remove nil params) "/")))
    (format nil "~a~a~(~a~)/~a"
            +endpoint+
            +simple-endpoint+
            method
            query-string)))

(defun strip-html-comments (content)
  "Remove any HTML comments from CONTENT.
The simple API now adds HTML comments to the end of the response, so they need
to be manually removed."
  (subseq content 0 (position #\< content)))
