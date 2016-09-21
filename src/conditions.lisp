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


