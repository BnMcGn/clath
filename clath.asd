;;;; clath.asd

(asdf:defsystem #:clath
  :description "Clath is single sign-on middleware for Clack. It allows basic login with OAuth1.0a, OAuth2 and OpenID Connect."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:ningle
               #:clack
               #:alexandria
               #:cl-hash-util
               #:drakma
               #:flexi-streams
               #:cl-json
               #:jose
               #:cl-who
               #:ubiquitous
               #:north)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "providers")
               (:file "bottom")
               (:file "clath")))

