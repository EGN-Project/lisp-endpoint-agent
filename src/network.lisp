;; Sample code using usocket for TCP and UDP, and trivial-sockets for multicast/broadcast

;; Install necessary libraries using Quicklisp
(ql:quickload '(:usocket :trivial-sockets))

;; TCP Communication using usocket
(defun start-tcp-server (port)
  ;; Create a TCP server socket and start listening on the specified port
  (let ((socket (usocket:socket-listen "0.0.0.0" port)))
    (format t "TCP Server started on port ~a~%" port)
    ;; Continuously accept incoming TCP connections and handle them
    (loop (handle-tcp-connection (usocket:socket-accept socket)))))

(defun handle-tcp-connection (client-socket)
  ;; Handle incoming TCP connections
  (format t "Handling TCP connection from ~a~%" (usocket:socket-peer client-socket))
  
  
  ;; Example: Read data from the client
  (let ((data (make-array 1024 :element-type 'base-char)))
    (usocket:socket-receive client-socket data)
    (format t "Received TCP data: ~a~%" data))
  
  ;; Close the client socket
  (usocket:socket-close client-socket))

;; UDP Communication using usocket
(defun start-udp-server (port)
  ;; Create a UDP server socket and bind it to the specified port
  (let ((socket (usocket:socket-bind "0.0.0.0" port :type :datagram)))
    (format t "UDP Server started on port ~a~%" port)
    ;; Continuously handle incoming UDP messages
    (loop (handle-udp-message (make-array 1024 :element-type 'base-char) (usocket:socket-receive-from socket)))))

(defun handle-udp-message (message remote-info)
  ;; Handle incoming UDP messages
  (format t "Received UDP message ~a from ~a~%" message remote-info))

;; Multicast/Broadcast using trivial-sockets
(defun start-multicast-listener (group port)
  ;; Create a multicast socket and start listening on the specified group and port
  (let ((socket (trivial-sockets:make-multicast-socket :group group :port port)))
    (format t "Multicast listener started on group ~a and port ~a~%" group port)
    ;; Continuously handle incoming multicast messages
    (loop (handle-multicast-message (trivial-sockets:socket-read-sequence socket)))))

(defun handle-multicast-message (message)
  ;; Handle incoming multicast messages
  (format t "Received multicast message ~a~%" message))

;; Start your servers
(start-tcp-server 5000)
(start-udp-server 6000)
(start-multicast-listener "239.0.0.1" 7000)
