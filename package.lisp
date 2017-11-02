;;;; package.lisp

(defpackage #:clath
  (:use #:cl #:cl-who #:alexandria)
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
   #:*login-destination-hook*
   #:clath
   #:provider-url-string
   #:initialize-secrets
   #:page-wrapper
   #:login-page
   #:clath-page-wrapper))

