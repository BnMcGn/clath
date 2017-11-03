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

(in-package #:cl-user)
(defpackage #:cljwt-custom
  (:use #:cl
        #:cl-base64)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:flexi-streams
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:ironclad
                #:make-hmac
                #:update-hmac
                #:hmac-digest
                #:encrypt-message)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:issue
           #:to-unix-time
           #:from-unix-time
           #:unsecured-token
           #:invalid-hmac
           #:invalid-rs256-signature
           #:unsupported-algorithm
           #:invalid-time
           #:expired
           #:not-yet-valid
           #:unpack
           #:verify-timestamps
           #:verify
           #:mismatched-algorithms
           #:missing-algorithm))

(in-package #:cljwt-custom)

(unless (boundp '+sha256-prefix+)
  (defconstant +sha256-prefix+ #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20)))

(unless (boundp '+rs256-padding+)
  (defconstant +rs256-padding+
    (concatenate '(vector (unsigned-byte 8))
                 #(#x00 #x01)
                 (make-array '(74) :element-type '(unsigned-byte 8) :initial-element #xff)
                 #(#x00)
                 +sha256-prefix+)))

(defmacro bind-hash-tables (bindings &body body)
  `(let ,(loop for binding in bindings collect
              (list (car binding)
                    `(etypecase ,(cadr binding)
                       (hash-table ,(cadr binding))
                       (list (plist-hash-table ,(cadr binding)
                                               :test #'equal)))))
     ,@body))

(defmacro add-claims (hash &rest claims)
  `(progn ,@(loop for (key value) on claims by #'cddr collect
                 `(when ,value
                    (setf (gethash ,key ,hash) ,value)))))

(defun to-unix-time (time)
  "Convert universal time to New Jersey time"
  (when time (- time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun from-unix-time (time)
  "Convert New Jersey time to universal time"
  (when time (+ time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun base64-encode (input)
  "Takes a string or octets, returns an unpadded URI-encoded Base64 string."
  (etypecase input
    (string (base64-encode (string-to-octets input :external-format :utf-8)))
    ((simple-array (unsigned-byte 8))
     (with-output-to-string (out)
       (with-input-from-string (in (usb8-array-to-base64-string input :uri t))
         (loop for character = (read-char in nil)
            while character do
            ;; CL-BASE64 always uses padding, which must be removed.
              (unless (eq character #\.)
                (write-char character out))))))))

(defun base64-decode (base-64-string)
  "Takes a base64-uri string and return an array of octets"
  (base64-string-to-usb8-array
   ;; Re-pad the string, or CL-BASE64 will get confused
   (concatenate 'string
                base-64-string
                (make-array (rem (length base-64-string) 4)
                            :element-type 'character
                            :initial-element #\.))
   :uri t))

(defun issue (claims &key algorithm key issuer subject audience
                       expiration not-before issued-at id more-header)
  "Encodes and returns a JSON Web Token. Times are in universal-time,
number of seconds from 1900-01-01 00:00:00"
  (bind-hash-tables ((claimset claims)
                     (header more-header))
    ;; Add registered claims to the claims hash table
    (add-claims claimset
                "iss" issuer
                "sub" subject
                "aud" audience
                "exp" (to-unix-time expiration)
                "nbf" (to-unix-time not-before)
                "iat" (to-unix-time issued-at)
                "jti" id)
    ;; Add type and algorithm to the header hash table
    (add-claims header
                "typ" "JWT"
                "alg" (ecase algorithm
                        (:none "none")
                        (:hs256 "HS256")
                        (:rs256 "RS256")))
    ;; Prepare JSON
    (let ((header-string (base64-encode
                          (with-output-to-string (s)
                            (yason:encode header s))))
          (claims-string (base64-encode
                          (with-output-to-string (s)
                            (yason:encode claimset s)))))
      ;; Assemble and, if applicable, sign the JWT
      (format nil "~A.~A.~@[~A~]"
              header-string
              claims-string
              (ecase algorithm
                (:none nil)
                (:hs256 (HS256-digest header-string
                                      claims-string
                                      key))
                (:rs256 (RS256-digest header-string
                                      claims-string
                                      key)))))))

(defun HS256-digest (header-string claims-string secret)
  "Takes header and claims in Base64, secret as a string or octets,
returns the digest, in Base64"
  (base64-encode
   (hmac-digest
    (update-hmac
     (make-hmac (etypecase secret
                  ((simple-array (unsigned-byte 8))
                   secret)
                  (string
                   (string-to-octets secret
                                     :external-format :utf-8)))
                'ironclad:SHA256)
     (concatenate '(vector (unsigned-byte 8))
                  (string-to-octets
                   header-string)
                  #(46) ; ASCII period (.)
                  (string-to-octets
                   claims-string))))))

(defun RS256-clear-padded-digest (header-string claims-string)
  "Takes header and claims in Base64 returns the non-crypted, padded digest, as octets"
  (concatenate '(vector (unsigned-byte 8))
               +rs256-padding+
               (ironclad:digest-sequence
                :sha256
                (concatenate '(vector (unsigned-byte 8))
                             (string-to-octets
                              header-string)
                             #(46)      ; ASCII period (.)
                             (string-to-octets
                              claims-string)))))


(defun RS256-digest (header-string claims-string private-key)
  "Takes header and claims in Base64, private-key as an ironclad rsa private-key,
returns the digest, in Base64"
  (base64-encode (encrypt-message
                  private-key
                  (RS256-clear-padded-digest header-string claims-string))))


(defun compare-HS256-digest (header-string claims-string
                             secret reported-digest)
  "Takes header and claims in Base64, secret as a string or octets, and a digest in Base64 to compare with. Signals an error if there is a mismatch."
  (let ((computed-digest
         (HS256-digest header-string
                       claims-string
                       secret)))
    (unless (equalp computed-digest
                    reported-digest)
      (cerror "Continue anyway" 'invalid-hmac
              :reported-digest reported-digest
              :computed-digest computed-digest))))

(defun compare-RS256-digest (header-string claims-string
                             public-key reported-digest)
  "Takes header and claims in Base64, public-key as an ironclad rsa private-key, and a
digest in Base64 to compare with. Signals an error if there is a mismatch."
  (let ((computed-clear-digest
         (RS256-clear-padded-digest header-string
                                    claims-string))
        (reported-clear-digest
         (encrypt-message
          public-key
          (base64-decode reported-digest))))
    ;; compare from 1 in the computer-clear-digest, as the encryption procedure
    ;; discards the first null byte
    (if (mismatch computed-clear-digest
                  reported-clear-digest :start1 1)
        (cerror "Continue anyway" 'invalid-rs256-signature
                :reported-digest reported-clear-digest
                :computed-digest computed-clear-digest))))

(defun unpack (jwt-string)
  "Returns 5 values: claims and header as hash tables. digest, claims and header in string form."
  (destructuring-bind (header claims digest)
      (split-sequence #\. jwt-string)
    (values
     (yason:parse
      (octets-to-string
       (base64-decode
        claims)
       :external-format :utf-8))
     (yason:parse
      (octets-to-string
       (base64-decode
        header)
       :external-format :utf-8))
     digest claims header)))

(defun verify-timestamps (claims)
  "Call with the first value returned by the unpack function."
  (let ((expires (from-unix-time (gethash "exp" claims)))
        (not-before (from-unix-time (gethash "nbf" claims)))
        (current-time (get-universal-time)))
    (when (and expires (> current-time expires))
      (cerror "Continue anyway" 'expired :delta (- current-time expires)))
    (when (and not-before (< current-time not-before))
      (cerror "Continue anyway" 'not-yet-valid :delta (- current-time not-before))))
  t)

;;According to
;;https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/
;;the JWT should not be allowed to decide the algorithm that verifies it.


(defun verify (jwt-string key algorithm
               &key fail-if-unsecured (fail-if-unsupported t))
  "Decodes and verifies a JSON Web Token. Returns two hash tables,
token claims and token header. The key and algorithm specifier should both be
supplied by the source of the token."
  (multiple-value-bind (claims header digest claims-string header-string)
      (unpack jwt-string)
    (if algorithm
        (unless (equal algorithm (gethash "alg" header))
          (cerror "Continue anyway" 'mismatched-algorithms
                  :specified-algorithm algorithm
                  :token-field-algorithm (gethash "alg" header)))
        (progn
          (cerror "Continue with token-specified algorithm" 'missing-algorithm)
          (setf algorithm (gethash "alg" header))))
    (cond ((equal algorithm "HS256")
           (compare-HS256-digest header-string
                                 claims-string
                                 key
                                 digest))
          ((equal algorithm "RS256")
           (compare-RS256-digest header-string
                                 claims-string
                                 key
                                 digest))
          ((and (or (null algorithm) (equal algorithm "none")) fail-if-unsecured)
           (cerror "Continue anyway" 'unsecured-token))
          (fail-if-unsupported (cerror "Continue anyway" 'unsupported-algorithm
                                       :algorithm algorithm)))
    (verify-timestamps claims)
    (values claims header)))

;;; Conditions

(define-condition unsecured-token (error) ())

(define-condition invalid-hmac (error) ())

(define-condition invalid-rs256-signature (error) ())

(define-condition mismatched-algorithms (error) ())

(define-condition missing-algorithm (error) ())

(define-condition unsupported-algorithm (error)
  ((algorithm :initarg :algorithm :reader algorithm))
  (:report (lambda (condition stream)
             (format stream "Algorithm \"~A\" not supported"
                     (algorithm condition)))))

(define-condition invalid-time (error)
  ((delta :initarg :delta :reader time-delta))
  (:report (lambda (condition stream)
             (format stream "Token ~A. ~D seconds off."
                     (typecase condition
                       (expired "has expired")
                       (not-yet-valid "is not yet valid"))
                     (time-delta condition)))))

(define-condition expired (invalid-time) ())

(define-condition not-yet-valid (invalid-time) ())
