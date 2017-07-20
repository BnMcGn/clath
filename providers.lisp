(in-package :clath)

(defun check-for-error (parameters)
  (alexandria:when-let
      ((err (assoc :error parameters))
       (err-msg (assoc :error--description parameters)))
    (error
     (format nil "OpenID Server Error: ~a, ~a" (cdr err) (cdr err-msg))))
  parameters)

(defparameter *provider-info*
  `(
    :facebook
    (:string "facebook"
     :auth-endpoint "https://www.facebook.com/dialog/oauth"
     :token-endpoint "https://graph.facebook.com/v2.3/oauth/access_token"
     :userinfo-endpoint "https://graph.facebook.com/v2.3/me"
     :auth-scope "email"
     :redirect-uri)
    :google
    (:string "google"
     :endpoints-url
     "https://accounts.google.com/.well-known/openid-configuration"
     :auth-scope "openid profile email")
    :github
    (:string "github"
     :auth-endpoint "https://github.com/login/oauth/authorize"
     :token-endpoint "https://github.com/login/oauth/access_token"
     :userinfo-endpoint "https://api.github.com/user"
     ;;FIXME: email endpoint not implemented yet.
     :email-endpoint "https://api.github.com/user/emails"
     :auth-scope "")
    :reddit
    (:string "reddit"
     :auth-endpoint "https://www.reddit.com/api/v1/authorize"
     :token-endpoint "https://www.reddit.com/api/v1/access_token"
     :userinfo-endpoint "https://oauth.reddit.com/api/v1/me"
     :auth-scope "identity")
    :stackexchange
    (:string "stackexchange"
     :auth-endpoint "https://stackexchange.com/oauth"
     :token-endpoint "https://stackexchange.com/oauth/access_token"
     ;:userinfo-endpoint "https://api.stackexchange.com/2.1/me"
     :userinfo-endpoint "https://api.stackexchange.com/2.1/me?site=stackoverflow"
     :auth-scope "")))

(defparameter *provider-secrets* nil)


