;;; -*- mode: lisp; -*-

(in-package :cl-user)

(defpackage :curl-easy-asd
  (:use :cl :asdf))

(in-package :curl-easy-asd)

(asdf:defsystem :curl-easy
  :description "Very primitive CFFI bindings for the libCURL easy API"
  :version "0.0.1"
  :author "Victor Anyakin <anyakinvictor@yahoo.com>"
  :licence "BSD"
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
		 (:file "curl-easy"))))
  :depends-on (:cffi))

;; EOF
