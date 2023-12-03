(asdf:defsystem #:project
  :version "0.1.0"
  :serial t
  :description "Endpoint agent project for EGN"
  :author ("Colton Rohan" "Emmanuel David" "Carlodavid Soto" "Vladia Zouga" "Nikhil Shetty" "Yurixander Ricardo")
  :license "MIT"
  :depends-on (:quicklisp)
  :components ((:module "src"
                        :components ((:file "main")))))

(asdf:defsystem #:quicklisp
  :version "0.1"
  :description "Quicklisp integration for the project"
  :author "Your Name"
  :license "MIT"
  :components ((:file "path-to-quicklisp-setup.lisp")))