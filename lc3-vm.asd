;;;; lc3-vm.asd

(asdf:defsystem #:lc3-vm
  :description "A VM to run the educational 'LC3' virtual machine"
  :author "blake@yellowpapersun.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "lc3-vm")))
