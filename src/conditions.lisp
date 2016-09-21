;;; conditions.lisp -- Error conditions that can be raised by the API

(in-package #:dogechain-api)


;; ----------------------------------------
;; -- Standard Error Conditions

(define-condition dogechain-api-error (error)
  ((message
    :initarg :message
    :accessor dogechain-api-error-message
    :initform nil
    :documentation "Message from the server indicating the error.")
   (url
    :initarg :url
    :accessor dogechain-api-error-url
    :initform nil
    :documentation "The API URL that was queried."))
  (:documentation "Standard error message all api errors extend."))

(defun dogechain-api-error (message &key url)
  "Throw an api error with MESSAGE and optional URL."
  (error 'dogechain-api-error
         :message message
         :url (if (null url) *last-called-url* url)))


;; ----------------------------------------
;; -- Address error conditions

(define-condition invalid-address-error (dogechain-api-error)
  ((address
    :initarg :address
    :accessor invalid-address-error-address
    :initform nil
    :documentation "The address that was marked as invalid."))
  (:documentation "Error raised when an address is invalid."))

(defun invalid-address-error (address &key url)
  "Throw an api error with MESSAGE and optional URL."
  (error 'invalid-address-error
         :message "Address is invalid"
         :address address
         :url (if (null url) *last-called-url* url)))
