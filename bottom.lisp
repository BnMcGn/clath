(in-package :clack-openid-connect)

(defparameter *callback-extension* "callback/")
(defparameter *login-extension* "login/")
(defvar *server-url*)

(setf drakma:*text-content-types*
      (cons '("application" . "json") drakma:*text-content-types*))

(defun discover-endpoints (discovery-url)
  (let ((disc (cl-json:decode-json-from-string (drakma:http-request discovery-url))))
    (list
     :auth-endpoint (gethash "authorization_endpoint" disc)
     :token-endpoint (gethash "token_endpoint" disc)
     :userinfo-endpoint (gethash "userinfo_endpoint" disc))))

;;;FIXME: Endpoint discovery only done on startup. Should look at spec and see if it should
;;;happen more frequently.
(defun provider-info (provider)
  (let ((prov (getf *provider-info* provider)))
    (unless prov (error "Not a recognized provider"))
    (if (getf prov :auth-endpoint)
        (extract-keywords
         '(:auth-endpoint :token-endpoint :userinfo-endpoint)
         prov)
        (progn
          (unless (getf prov :endpoints-url)
            (error "Provider must have :endpoints-url or endpoint definitions"))
          (let ((res (discover-endpoints (getf prov :endpoints-url))))
             (setf (getf *provider-info* provider)
                   (concatenate 'list prov res))
             res)))))

(defun provider-secrets (provider)
  (getf *provider-secrets* provider))

(defun provider-string (provider)
  (aif (getf (getf *provider-info* provider) :string)
       it
       (string-downcase (string provider))))

(defun make-login-url (provider)
  (concatenate 'string *server-url* *login-extension* (provider-string provider)))

(defun make-callback-url (provider)
  (concatenate 'string *server-url* *callback-extension* (provider-string provider)))

(defun available-providers ()
  (remove-if-not #'keywordp *provider-secrets*))

;;;FIXME: Audit me: this number is probably correct/random enough, because the public
;;;*probably* never sees it. Should get a knowledgable opionion on it though.
(defun gen-state (len)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat len
         do (princ (random 36) stream)))))

(defun special-url-p (url-path)
  (or (sequence-starts-with (concatenate 'string "/" *callback-extension*) url-path)
      (sequence-starts-with (concatenate 'string "/" *login-extension*) url-path)))

(defun request-user-auth-destination
    (&key auth-scope client-id auth-endpoint state redirect-uri &allow-other-keys)
  (drakma:http-request
   auth-endpoint :redirect nil
   :parameters `(("client_id" . ,cliend-id) ("app_id" . ,client-id)
                 ("response_type" . "code") ("scope" . ,auth-scope)
                 ("redirect_uri" . ,redirect-uri) ("state" . ,state))))

;;;WARNING: Function saves state to session!
(defun login-action (provider)
  (let ((state (gen-state 36)))
    (setf (gethash 'state *session*) state)
    (setf (gethash :oid-connect-provider *session*) provider)
    (multiple-value-bind (content resp-code headers uri)
        (apply #'request-user-auth-destination :state state
               :redirect-uri (make-callback-url provider)
               :client-id (getf (provider-secrets provider) :client-id)
               (provider-info provider))
      (declare (ignore headers))
      (if (< resp-code 400) `(302 (:location ,(format nil "~a" uri)))
          content))))

;;;FIXME: Why does this need redirect_uri? Try without.
(defun request-access-token (provider code redirect-uri)
  (let ((info (provider-info provider))
        (secrets (provider-secret)))
    (funcall
     (getf info :token-processor)
     (cl-json:decode-json-from-string
      (drakma:http-request (getf :token-endpoint info)
                           :method :post
                           :redirect nil
                           :parameters `(("code" . ,code)
                                         ("client_id" . ,(getf :client-id secrets))
                                         ("app_id" . ,(getf :client-id secrets))
                                         ("client_secret" . ,(getf :secret secrets))
                                         ("redirect_uri" . ,redirect-uri)
                                         ("grant_type" . "authorization_code")))))))

(defun request-user-info (provider access-token)
  ;;FIXME: Doesn't handle failure
  (cl-json:decode-json-from-string
   (drakma:http-request (getf (provider-info provider) :userinfo-endpoint)
                        :parameters `(("alt" . "json") ;Facebook might not like alt param?
                                      ("access_token" . ,access-token)))))

(defun valid-state (received-state)
  (equal (gethash 'state *session*) received-state))

(defun destination-on-login ()
  (aif (gethash :oid-connect-destination *session*)
       it
       "/"))

(defun callback-action (provider parameters)
  ;;Can we check state yet?
  (if (not (valid-state (assoc-cdr "state" parameters #'equal)))
      '(403 '() "Out, vile imposter!")
      (multiple-value-bind (access-token id-token)
          (request-access-token
           provider (assoc-cdr "code" parameters #'equal) (make-callback-url provider))
        (with-keys (:oid-connect-access-token :oid-connect-userinfo :oid-connect-id-token)
            *session*
          (setf oid-connect-access-token access-token
                oid-connect-userinfo (request-user-info provider access-token)
                oid-connect-id-token id-token))
        '(302 (:location (destination-on-login))))))

(defun logout-action ()
  (remhash 'state *session*)
  (remhash :oid-connect-provider *session*)
  (remhash :oid-connect-access-token *session*)
  (remhash :oid-connect-userinfo *session*)
  (remhash :oid-connect-connect-id-token *session*))





