(in-package :clack-openid-connect)

(defparameter *callback-extension* "callback/")
(defparameter *login-extension* "login/")
(defvar *server-url*)

(setf drakma:*text-content-types*
      (cons '("application" . "json") drakma:*text-content-types*))

(defun discover-endpoints (discovery-url)
  (let ((disc (cl-json:decode-json-from-string (drakma:http-request discovery-url))))
    (list
     :auth-endpoint (assoc-cdr :authorization--endpoint disc)
     :token-endpoint (assoc-cdr :token--endpoint disc)
     :userinfo-endpoint (assoc-cdr :userinfo--endpoint disc))))

;;;FIXME: Endpoint discovery only done on startup. Should look at spec and see if it should
;;;happen more frequently.
(defun provider-info (provider)
  (let ((prov (getf *provider-info* provider)))
    (unless prov (error "Not a recognized provider"))
    (if (getf prov :auth-endpoint)
        (alexandria:alist-plist
         (extract-keywords
          '(:auth-endpoint :token-endpoint :userinfo-endpoint :auth-scope
            :token-processor)
          prov))
        (progn
          (unless (getf prov :endpoints-url)
            (error "Provider must have :endpoints-url or endpoint definitions"))
          (let ((res (discover-endpoints (getf prov :endpoints-url))))
             (setf (getf *provider-info* provider)
                   (concatenate 'list prov res)))))))

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
   :parameters `(("client_id" . ,client-id) ("app_id" . ,client-id)
                 ("response_type" . "code") ("scope" . ,auth-scope)
                 ("redirect_uri" . ,redirect-uri) ("state" . ,state))))

;;;WARNING: Function saves state to session!
(defun login-action (provider)
  (unless (ningle:context :session)
    (setf (ningle:context :session) (make-hash-table)))
  (let ((state (gen-state 36)))
    (setf (gethash 'state (ningle:context :session)) state)
    (setf (gethash :oid-connect-provider (ningle:context :session)) provider)
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
        (secrets (provider-secrets provider)))
    (multiple-value-bind (response code headers)
        (drakma:http-request
         (getf info :token-endpoint)
         :method :post
         :redirect nil
         :parameters `(("code" . ,code)
                       ("client_id" . ,(getf secrets :client-id))
                       ("app_id" . ,(getf secrets :client-id))
                       ("client_secret" . ,(getf secrets :secret))
                       ("redirect_uri" . ,redirect-uri)
                       ("grant_type" . "authorization_code")))
      (declare (ignore code))
      (let ((subtype (nth-value 1 (drakma:get-content-type headers))))
        (check-for-error
         (cond
           ((equal subtype "json")
            (cl-json:decode-json-from-string response))
           ((equal subtype "x-www-form-urlencoded")
            (quri:url-decode-params
             (flexi-streams:octets-to-string response)))))))))

(defun get-access-token (atdata)
  (alexandria:if-let ((atok (assoc :access--token atdata)))
    (cdr atok)
    (cdr (assoc "access_token" atdata :test #'equal))))

(defun get-id-token (atdata)
  (alexandria:if-let ((itok (assoc :id--token atdata)))
    ;;FIXME: Perhaps should be downloading key and verifying JWT
    (let ((claims (cljwt:unpack (cdr itok))))
      (cljwt:verify-timestamps claims)
      claims)
    (get-access-token atdata)))

(defun request-user-info (provider access-token)
  ;;FIXME: Doesn't handle failure
  (cl-json:decode-json-from-string
   (drakma:http-request (getf (provider-info provider) :userinfo-endpoint)
                        :parameters `(("alt" . "json") ;Facebook might not like alt param?
                                      ("access_token" . ,access-token)))))

(defun valid-state (received-state)
  (and (ningle:context :session)
       (equal (gethash 'state (ningle:context :session)) received-state)))

;;;FIXME: oidc shouldn't be handling destination/redirect. It's a more general
;;; problem. *login-destination* is a temporary hack to deal with that.

(defvar *login-destination* nil)
(defparameter *login-destination-hook* nil)

(defun destination-on-login ()
  (if (functionp *login-destination-hook*)
      (funcall *login-destination-hook*
               :username (gethash :username (ningle:context :session)))
      (if *login-destination* *login-destination*
          (aif (gethash :oid-connect-destination (ningle:context :session))
               it
               "/"))))

(defun callback-action (provider parameters &optional post-func)
  ;;Can we check state yet?
  (if (not (valid-state (assoc-cdr "state" parameters #'equal)))
      '(403 '() "Login failed. State mismatch.")
      (let* ((at-data (request-access-token
                       provider
                       (assoc-cdr "code" parameters #'equal)
                       (make-callback-url provider)))
             (access-token (get-access-token at-data)))
        (with-keys (:oid-connect-access-token :oid-connect-userinfo
                                              :oid-connect-id-token)
            (ningle:context :session)
          (setf oid-connect-access-token access-token
                oid-connect-userinfo (request-user-info provider access-token)
                oid-connect-id-token (get-id-token at-data)))
        (when (functionp post-func) (funcall post-func))
        `(302 (:location ,(destination-on-login))))))

(defun logout-action ()
  (remhash 'state (ningle:context :session))
  (remhash :oid-connect-provider (ningle:context :session))
  (remhash :oid-connect-access-token (ningle:context :session))
  (remhash :oid-connect-userinfo (ningle:context :session))
  (remhash :oid-connect-connect-id-token (ningle:context :session)))





