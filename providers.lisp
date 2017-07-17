(in-package :clack-openid-connect)

(defun check-for-error (parameters)
  (alexandria:when-let
      ((err (assoc :error parameters))
       (err-msg (assoc :error--description parameters)))
    (error
     (format nil "OpenID Server Error: ~a, ~a" (cdr err) (cdr err-msg))))
  parameters)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun default-token-processor (parameters)
    (check-for-error parameters)
    (let ((access-token (cdr (assoc :access--token parameters))))
      (values access-token access-token)))

  (defun google-token-processor (parameters)
    (check-for-error parameters)
    (values (cdr (assoc :access--token parameters))
            (cljwt:decode
             (cdr (assoc :id--token parameters))
             :fail-if-unsupported nil))))

(defparameter *provider-info*
  `(
    :facebook
    (:string "facebook"
     :auth-endpoint "https://www.facebook.com/dialog/oauth"
     :token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token"
     :userinfo-endpoint "https://graph.facebook.com/v2.3/me"
     :auth-scope "email"
     :redirect-uri
     :token-processor ,(function default-token-processor))
    :google
    (:string "google"
     :endpoints-url
     "https://accounts.google.com/.well-known/openid-configuration"
     :auth-scope "openid profile email"
     :token-processor ,(function google-token-processor))
    :github
    (:string "github"
     :auth-endpoint "https://github.com/login/oauth/authorize"
     :token-endpoint "https://github.com/login/oauth/access_token"
     :userinfo-endpoint "https://api.github.com/user"
     ;;FIXME: email endpoint not implemented yet.
     :email-endpoint "https://api.github.com/user/emails"
     :auth-scope ""
     :token-processor ,(function default-token-processor))))

(defparameter *provider-secrets* nil)

;;;Token processors, given the callback parameters as an alist, should return, as values,
;;; the access token, and the id token. Providers vary about what this means. Facebook,
;;; for example, uses the access-token as the id-token, whereas Google does something else.

