(ql:quickload "drakma")
(ql:quickload "cl-json")
(ql:quickload "babel")
(ql:quickload "flexi-streams")


(defun call-endpoint-api (url method &optional payload)
  (let ((response (drakma:http-request url
                                       :method method
                                       :parameters payload
                                       :content-type "application/json"
                                       :accept "application/json")))
    (cond ((typep response 'vector) ; if response is a byte array
           (babel:octets-to-string response :encoding :utf-8))
          ((typep response 'string) ; if it's already a string
           response)
          (t
           (error "Unexpected response type")))))

(defun deploy-deployment (deployment-id description author code)
  "Deploys a deployment by sending a POST request to the specified URL."
  (let* ((url "http://localhost:3000/deploy")
         (response (drakma:http-request
                    url
                    :method :post
                    :parameters `(("authorID" . ,author)
                                  ("comment" . ,description)
                                  ("payload" . ,code)
                                  ("deploymentID" . ,deployment-id))
                    :content-type "application/json"))
         (response-string (babel:octets-to-string response)))
    (format t "Response: ~a~%" response-string)))

(defun get-deployment-by-id (deployment-id)
  (let* ((url "http://localhost:3000/getDeploymentByID")
         (response (drakma:http-request
                    url
                    :method :post 
                    :parameters `(("deploymentID" . ,deployment-id))
                    :content-type "application/json"))
         (json-payload (babel:octets-to-string response)))
    (format t "Response: ~a~%" json-payload)))

(defun revoke-deployment (deployment-id reason authorID)
  (let* ((url "http://localhost:3000/revokeDeployment")
        (response (drakma:http-request
                    url
                    :method :post 
                    :parameters `(("deploymentID" . ,deployment-id)
                                  ("reason" . ,reason) 
                                  ("authorID" . ,authorID))
                    :content-type "application/json"))
         (json-payload (babel:octets-to-string response)))
    (format t "Response: ~a~%" response)))

(defun get-all-revocations ()
  (let* ((url "http://localhost:3000/getAllRevocations")
         (response (call-endpoint-api url :get))
         (json-payload (babel:octets-to-string response)))
    (format t "Response: ~a~%" json-payload)))

(defun get-revocation-by-id (revocation-id)
  (let* ((url "http://localhost:3000/getRevocationByID")
         (response (drakma:http-request
                    url
                    :method :post 
                    :parameters `(("revocationID" . ,revocation-id))
                    :content-type "application/json"))
         (json-payload (babel:octets-to-string response)))
    (format t "Response: ~a~%" json-payload)))

(defun validate-revocation (revocation-id)
  (let* ((url "http://localhost:3000/validateRevocation")
          (response (drakma:http-request
                    url
                    :method :post 
                    :parameters `(("revocationID" . ,revocation-id))
                    :content-type "application/json"))
         (json-payload (babel:octets-to-string response)))
    (format t "Response: ~a~%" response)))

(defun get-all-transaction-logs ()
  (let* ((url "http://localhost:3000/transaction-logs")
         (response (call-endpoint-api url :get)))
    (format t "Response: ~a~%" response)))

(defun endpoint-console ()
  (format t "Welcome to the Hyperledger Fabric API Console~%")
  (format t "Choose an option:~%")
  (format t "1. Deploy a deployment~%")
  (format t "2. Get deployment by ID~%")
  (format t "3. Revoke a deployment~%")
  (format t "4. Get all revocations~%")
  (format t "5. Get revocation by ID~%")
  (format t "6. Validate revocation~%")
  (format t "7. Get all transaction logs~%")
  (format t "Enter your choice (1-7): ")
  (finish-output)
  (let ((choice (read)))
    (case choice
      (1 (progn
           (format t "Enter deployment ID: ")
           (finish-output)
           (let ((deployment-id (read-line)))
             (format t "Enter description: ")
             (finish-output)
             (let ((description (read-line)))
               (format t "Enter author: ")
               (finish-output)
               (let ((author (read-line)))
                 (format t "Enter code: ")
                 (finish-output)
                 (let ((code (read-line)))
                   (deploy-deployment deployment-id description author code)))))))
      (2 (progn
           (format t "Enter deployment ID: ")
           (finish-output)
           (let ((deployment-id (read-line)))
             (get-deployment-by-id deployment-id))))
      (3 (progn
           (format t "Enter deployment ID to revoke: ")
           (finish-output)
           (let ((deployment-id (read-line)))
            (format t "Enter reason of revoke: ")
            (finish-output)
            (let ((reason (read-line)))
              (format t "Enter author ID of revoke: ")
              (finish-output)
              (let ((authorID (read-line)))
                (revoke-deployment deployment-id reason authorID))))))
      (4 (get-all-revocations))
      (5 (progn
           (format t "Enter revocation ID: ")
           (finish-output)
           (let ((revocation-id (read-line)))
             (get-revocation-by-id revocation-id))))
      (6 (progn
           (format t "Enter revocation ID to validate: ")
           (finish-output)
           (let ((revocation-id (read-line)))
             (validate-revocation revocation-id))))
      (7 (get-all-transaction-logs))
      (t (format t "Invalid choice. Please enter a number between 1 and 7.~%")))
    (format t "------------------------------------------~%")
    (format t "Do you want to perform another action? (y/n): ")
    (finish-output)
    (let ((continue (char-downcase (read-char))))
      (when (char= continue #\y)
        (endpoint-console)))))

(endpoint-console)
