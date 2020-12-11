(defpackage :curl-easy
  (:nicknames :curl)
  (:use :common-lisp :cffi)
  (:export
   :reset-handle
   :easy-query
   :reuse-query))

(in-package :curl-easy)

(define-foreign-library libcurl
    (:darwin (:or "libcurl.3.dylib" "libcurl.dylib"))
  (:unix (:or "libcurl.so.3" "libcurl.so"))
  (t (:default "libcurl")))

(use-foreign-library libcurl)
;; EOF
