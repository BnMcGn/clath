;;;; package.lisp

(defpackage #:clack-openid-connect
  (:use #:cl #:cl-who #:anaphora)
  (:import-from #:cl-hash-util
                #:with-keys)
  (:export
   #:*callback-extension*
   #:*login-extension*
   #:available-providers
   #:component
   #:*login-destination*
   #:login-url
   #:logout-url
   #:*login-destination-hook*))

