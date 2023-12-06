(defpackage :private-blockchain
  (:use :common-lisp :ironclad))

(in-package :private-blockchain)

; Defines the structure for a block in the blockchain. It includes properties like index, timestamp, transactions, previous hash, and hash.
(defstruct block
  (index 0 :type fixnum)
  (timestamp 0 :type fixnum)
  (transactions '())
  (previous-hash "")
  (hash ""))

;  Defines the structure for a transaction. It includes properties like ID, author ID, and timestamp.
(defstruct transaction
  (id "")
  (authorId "")
  (timestamp 0 :type fixnum))

; Defines a class for deployment, which extends the transaction structure by adding properties for code and comments.
(defclass deployment (transaction)
  (code "")
  (comment "" :type (or string null)))

; A utility function that calculates the SHA-256 hash of input data using Ironclad library functions.
(defun calculate-hash (data)
  (ironclad:byte-array-to-hex-string
  (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array data))))

; Creates the initial block (genesis block) of the blockchain with index 0, a timestamp, and a hash generated from the string "Genesis Block".
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

;; Add different transaction types with specific validations
(defun add-deployment-transaction (author-id code comment)
  "Adds a deployment transaction to the blockchain."
  (make-deployment-transaction :id (format nil "DEPLOY~a" (get-universal-time))
                               :author-id author-id
                               :timestamp (get-universal-time)
                               :code code
                               :comment comment))

; Validate different transaction types
(defun validate-deployment-transaction (transaction)
  "Validates a deployment transaction."
  (and (stringp (deployment-transaction-id transaction))
       (stringp (deployment-transaction-author-id transaction))
       (numberp (deployment-transaction-timestamp transaction))
       (stringp (deployment-transaction-code transaction))
       (or (stringp (deployment-transaction-comment transaction)) (null (deployment-transaction-comment transaction)))))

; Function to validate the entire blockchain
(defun validate-blockchain (blockchain)
  "Validates the entire blockchain."
  (loop for block in blockchain
        for next-block in (cdr blockchain)
        always (and (is-valid-block next-block block)  ; Validates block integrity
                    (= (block-index next-block) (+ 1 (block-index block))))))  ; Checks block indexing

; Function to add a transaction of any type to a block
(defun add-transaction-to-block (block transaction)
  "Adds a transaction of any type to a block."
  (let ((new-transactions (cons transaction (block-transactions block))))
    (setf (block-transactions block) new-transactions)
    (setf (block-hash block) (calculate-hash (format nil "~a~a" (block-previous-hash block) transaction))))
  block)

; Function to verify the validity of any transaction type
(defun validate-transaction (transaction)
  "Verifies the validity of a transaction of any type."
  (typecase transaction
    (deployment-transaction (validate-deployment-transaction transaction))
    ;; Add more cases for different transaction types if needed
    (t nil)))  ; Returns nil if the transaction type is not recognized

; Function to verify the validity of all transactions in a block
(defun validate-transactions-in-block (block)
  "Verifies the validity of all transactions in a block."
  (every #'validate-transaction (block-transactions block)))

; Function to validate the integrity of a block and its transactions
(defun is-valid-block-with-transactions (block previous-block)
  "Checks if a newly created block and its transactions are valid based on its previous block."
  (and (is-valid-block block previous-block)
       (validate-transactions-in-block block)))

