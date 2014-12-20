(in-package #:dogechain-api)

;; ----------------------------------------------------------------------
;; -- Configuration
;; ----------------------------------------------------------------------

(defparameter +endpoint+ "http://dogechain.info"
  "URI endpoint for Dogechain.info")

(defparameter +simple-endpoint+ "/chain/Dogecoin/q/"
  "Path to the simple endpoint.")

(defvar *last-called-url* ""
  "The last URL that was queried.")


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-address-balance (address)
  "Get amount ever received minus amount ever sent by ADDRESS."
  (let ((response (get-simple "addressbalance" address)))
    (if (string= response "ERROR: address invalid")
        (api-error "Address invalid")
        (read-from-string response))))

(defun address-to-hash (address)
  "Get the public key hash for ADDRESS."
  (get-simple "addresstohash" address))


;; ----------------------------------------------------------------------
;; -- Error Handling
;; ----------------------------------------------------------------------

(define-condition api-error (error)
  ((message
    :initarg :message
    :accessor api-error-message
    :initform nil
    :documentation "Message from the server indicating the error.")
   (url
    :initarg :url
    :accessor api-error-url
    :initform nil
    :documentation "The API URL that was queried.")))

(defun api-error (message &key url)
  "Throw an api error with MESSAGE and optional URL."
  (error 'api-error
         :message message
         :url (if (null url) *last-called-url* url)))


;; ----------------------------------------------------------------------
;; -- Simple API Functions
;; ----------------------------------------------------------------------

(defun get-simple (method &rest params)
  "Get a plaintext result from the chain's METHOD with optional PARAMS."
  (let ((drakma:*header-stream* nil)
        (url (build-simple-endpoint method params)))
    (setf *last-called-url* url)
    (drakma:http-request url)))

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
