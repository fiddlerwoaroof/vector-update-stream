;;;; package.lisp

(defpackage :vector-update-stream
  (:use :cl :trivial-gray-streams)
  (:export #:make-update-stream))

