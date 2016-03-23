(in-package :clack-openid-connect)

(defparameter *provider-info*
  '(
    :facebook
    (:string "facebook"
     :auth-endpoint "https://www.facebook.com/dialog/oauth"
     :token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token"
     :userinfo-endpoint "https://graph.facebook.com/v2.3/me"
     :auth-scope "email"
     :redirect-uri
     :token-processor #'default-token-processor)
    :google
    (:string "google"
     :endpoints-url
     "https://accounts.google.com/.well-known/openid-configuration"
     :auth-scope "openid profile email"
     :token-processor #'google-token-processor)))

(defparameter *provider-secrets* nil)

;;;Token processors, given the callback parameters as an alist, should return, as values,
;;; the access token, and the id token. Providers vary about what this means. Facebook,
;;; for example, uses the access-token as the id-token, whereas Google does something else.

(defun default-token-processor (parameters)
  (let ((access-token (cdr (assoc :access--token parameters))))
    (values access-token access-token)))

(defun google-token-processor (parameters)
  (values (cdr (assoc :access--token parameters))
          (cljwt:decode
           (cdr (assoc :id--token parameters))
           :fail-if-unsupported nil)))
