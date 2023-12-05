(defpackage :private-blockchain
  (:use :common-lisp :ironclad))

(in-package :private-blockchain)

(defstruct block
  (index 0 :type fixnum)
  (timestamp 0 :type fixnum)
  (transactions '())
  (previous-hash "")
  (hash ""))

(defstruct transaction
  (id "")
  (authorId "")
  (timestamp 0 :type fixnum))

(defclass deployment (transaction)
  (code "")
  (comment "" :type (or string null)))

(defun calculate-hash (data)
  (ironclad:byte-array-to-hex-string
  (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array data))))

(defun create-genesis-block ()
  (make-block :index 0
              :timestamp (get-universal-time)
              :transactions '("Genesis Transaction")
              :previous-hash "0"
              :hash (calculate-hash "Genesis Block")))
