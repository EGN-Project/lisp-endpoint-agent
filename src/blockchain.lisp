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

; A function to add a new transaction to a given block. It appends the transaction to the block's list of transactions and recalculates the block's hash.
(defun add-transaction (block transaction)
  (let ((new-transactions (cons transaction (block-transactions block))))
    (setf (block-transactions block) new-transactions)
    (setf (block-hash block) (calculate-hash (format nil "~a~a" (block-previous-hash block) transaction)))))

(defun create-new-block (previous-block transactions)
  "Creates a new block given the previous block and list of transactions."
  (let ((new-block (make-block :index (+ 1 (block-index previous-block))
                               :timestamp (get-universal-time)
                               :transactions transactions
                               :previous-hash (block-hash previous-block)
                               :hash "")))
    (setf (block-hash new-block) (calculate-hash (format nil "~a~a" (block-previous-hash new-block) transactions)))
    new-block))

(defun is-valid-block (new-block previous-block)
  "Checks if a newly created block is valid based on its previous block."
  (and (= (block-index new-block) (+ 1 (block-index previous-block)))
       (string= (block-previous-hash new-block) (block-hash previous-block))
       (= (calculate-hash (format nil "~a~a" (block-previous-hash new-block) (block-transactions new-block)))
          (block-hash new-block))))

(defun add-block (new-block blockchain)
  "Adds a new block to the blockchain if it's valid."
  (if (is-valid-block new-block (car blockchain))
      (cons new-block blockchain)
      blockchain))

;; Extend the transaction structure to accommodate different types
(defstruct deployment-transaction
  (id "")
  (author-id "")
  (timestamp 0 :type fixnum)
  (code "")
  (comment "" :type (or string null)))
