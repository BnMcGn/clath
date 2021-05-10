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
   #:clath-page-wrapper
   #:clath-login-page
   #:clath-not-logged-page
   #:clath-logout-page
   #:*in-logout-page*
   #:disable-provider
   #:enable-provider
   #:provider-disabled?
   #:login-links))

