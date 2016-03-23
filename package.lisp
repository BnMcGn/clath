;;;; package.lisp

(defpackage #:clack-openid-connect
  (:use #:cl #:cl-who #:anaphora)
  (:import-from #:ningle #:*session*)
  (:export
   #:*callback-extension*
   #:*login-extension*
   #:available-providers
   #:app))

