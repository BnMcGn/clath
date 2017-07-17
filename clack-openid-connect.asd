;;;; clack-openid-connect.asd

(asdf:defsystem #:clack-openid-connect
  :description "Describe clack-openid-connect here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:ningle
               #:clack
               #:alexandria
               #:cl-hash-util
               #:drakma
               #:flexi-streams
               #:cl-json
               #:cljwt
               #:cl-who
               #:ubiquitous
               #:anaphora
               #:webhax)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "providers")
               (:file "bottom")
               (:file "clack-openid-connect")))

