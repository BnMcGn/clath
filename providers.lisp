(in-package :clath)

(defun check-for-error (parameters)
  (when-let
      ((err (assoc :error parameters))
       (err-msg (assoc :error--description parameters)))
    (error
     (format nil "OpenID Server Error: ~a, ~a" (cdr err) (cdr err-msg))))
  parameters)

(defparameter *provider-info*
  `(
    :facebook
    (:string "facebook"
     :auth-endpoint ""
     :token-endpoint "https://graph.facebook.com/v2.10/oauth/access_token")
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
     :userinfo-endpoint
     "https://api.stackexchange.com/2.1/me?site=stackoverflow"
     :auth-scope "")
    :twitter
    (:string "twitter"
     :use-north t
     :auth-endpoint "https://api.twitter.com/oauth/authorize"
     :access-endpoint "https://api.twitter.com/oauth/access_token"
     :request-endpoint "https://api.twitter.com/oauth/request_token"
     :userinfo-endpoint
     "https://api.twitter.com/1.1/account/verify_credentials.json"
     :auth-scope "")
    :linkedin
    (:string
     "linkedin"
     :auth-endpoint "https://www.linkedin.com/uas/oauth2/authorization"
     :token-endpoint "https://www.linkedin.com/uas/oauth2/accessToken"
     :userinfo-endpoint "https://api.linkedin.com/v1/people/~"
     :auth-scope "")
    :yahoo
    (:string "yahoo"
     :auth-endpoint "https://api.login.yahoo.com/oauth2/request_auth"
     :token-endpoint "https://api.login.yahoo.com/oauth2/get_token"
     :userinfo-endpoint ""
     :auth-scope "identity"
     :url-string "yh")))

(defparameter *provider-secrets* nil)

(defgeneric request-user-info (provider access-token))

(defmethod request-user-info ((provider t) access-token)
  ;;FIXME: Doesn't handle failure
  (cl-json:decode-json-from-string
   (drakma:http-request (getf (provider-info provider) :userinfo-endpoint)
                        ;;Facebook might not like alt param?
                        :parameters `(("alt" . "json")
                                      ("access_token" . ,access-token))
                        :basic-authorization (basic-authorization provider)
                        :user-agent (user-agent provider))))

(defmethod request-user-info ((provider (eql :reddit)) access-token)
  (cl-json:decode-json-from-string
   (drakma:http-request (getf (provider-info provider) :userinfo-endpoint)
                        :parameters `(("access_token" . ,access-token))
                        :method :get
                        :additional-headers
                        `(("Authorization"
                           . ,(format nil "bearer ~a" access-token)))
                        :user-agent (user-agent provider))))

(defmethod request-user-info ((provider (eql :linkedin)) access-token)
  (cl-json:decode-json-from-string
   (drakma:http-request (getf (provider-info provider) :userinfo-endpoint)
                        :parameters `(("oauth2_access_token" . ,access-token)
                                      ("format" . "json"))
                        :method :get
                        :user-agent (user-agent provider))))

(defmethod request-user-info ((provider (eql :stackexchange)) access-token)
  (cadr
   (assoc
    :items
    (cl-json:decode-json-from-string
     (drakma:http-request  (getf (provider-info provider) :userinfo-endpoint)
                           :parameters `(("key" . ,(getf (provider-secrets provider)
                                                         :key))
                                         ("access_token" . ,access-token))
                           :basic-authorization (basic-authorization provider)
                           :decode-content t
                           :user-agent (user-agent provider))))))

;;For now we will hope that all OAuth1.0a providers will behave the same.
;;If not, this can be turned into generic-and-methods.
(defun request-user-info-north (provider north-client)
  (cl-json:decode-json-from-string
   (north:make-signed-request
    north-client
    (getf (provider-info provider) :userinfo-endpoint) :get)))

