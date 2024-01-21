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

(defclass smart-contract (transaction)
  (code "")
  (comment "" :type (or string null)))

(defun deploy-smart-contract (author-id code comment)
  "Deploy a new smart contract on the blockchain."
  (let* ((timestamp (get-universal-time))
         (contract-id (calculate-hash (format nil "~a~a~a" author-id timestamp code)))
         (deployment-transaction (make-instance 'deployment-transaction
                                               :id contract-id
                                               :author-id author-id
                                               :timestamp timestamp
                                               :code code
                                               :comment comment))
         (smart-contract (make-instance 'smart-contract
                                        :id contract-id
                                        :author-id author-id
                                        :timestamp timestamp
                                        :code code
                                        :comment comment)))
    (setf *blockchain* (add-transaction deployment-transaction *blockchain*)) ; Assuming add-transaction is defined
    (setf *blockchain* (add-transaction smart-contract *blockchain*))))


    ;; Example usage
(deploy-smart-contract "author123" "(define (add a b) (+ a b))" "Simple addition contract")

;; You can now interact with the deployed smart contract by extending the code
;; For example, add a function to invoke a method of the smart contract:
(defun invoke-contract-method (contract method args)
  "Invoke a method of a deployed smart contract."
  ;; Add your logic to interact with the smart contract and execute the specified method
  (format t "Invoking method ~a of smart contract ~a with args ~a~%" method (smart-contract-id contract) args))

;; Example usage
(let* ((contract (first *blockchain*)) ; Assuming the smart contract is the first transaction in the blockchain
       (method "add")
       (args '(3 5)))
  (invoke-contract-method contract method args))