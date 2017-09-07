;;;; clath.asd

(asdf:defsystem #:clath
  :description "Describe clath here"
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
               #:webhax
               #:north)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "providers")
               (:file "bottom")
               (:file "clath")))

