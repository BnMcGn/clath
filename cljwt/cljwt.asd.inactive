;; Copyright © 2014 Grim Schjetne <grim@schjetne.se>

;; This file is part of CLJWT.

;; CLJWT is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; CLJWT is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with CLJWT.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :cl-user)
(defpackage :cljwt-system (:use :cl :asdf))
(in-package :cljwt-system)

(defsystem cljwt
  :name "CLJWT"
  :author "Grim Schjetne <grim@schjetne.se"
  :description "JSON Web Token library"
  :license "LGPLv3+"
  :depends-on (:ironclad
               :yason
               :cl-base64
               :flexi-streams
               :split-sequence)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cljwt")))))
