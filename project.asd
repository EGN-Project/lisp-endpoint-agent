(asdf:defsystem #:project
  :version "0.1.0"
  :serial t
  :description "Endpoint agent project for EGN"
  :author ("Colton Rohan" "Emmanuel David" "Carlodavid Soto" "Vladia Zouga" "Nikhil Shetty" "Yurixander Ricardo")
  :license "MIT"
  :components ((:module "src"
                        :components ((:file "main")))))
