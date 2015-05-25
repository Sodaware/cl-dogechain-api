;; -*- mode: common-lisp -*-

;; Load main package
(ql:quickload '(:dogechain-api :split-sequence))
(import 'split-sequence:split-sequence)

#+sbcl(setf sb-impl::*default-external-format* :utf-8)
#+sbcl(setf sb-alien::*default-c-string-external-format* :utf-8)

;; Run all unit tests
(defun test ()
  (load "dogechain-api-test.asd")
  (asdf:test-system :dogechain-api))
