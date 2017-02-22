(in-package :clack-openid-connect)

#|

FIXME: Stuff in this file will need to be rethought when a more general
login manager is developed.

|#

(defparameter *openid-app-address* "/oid_connect")
(defparameter *logout-extension* "logout/")

;;;FIXME: *server-url* can't be dynamically set with let, because functions are not
;;; called from with in the app function, but rather by the ningle app. Therefore
;;; *server-url* is setf instead, so further instances of openid-app will stomp it.
;;; Single use only for now.

(defun login-app (base-url)
  (setf *server-url* base-url)
  (initialize-secrets)
  (let ((app (make-instance 'ningle:<app>)))
    (dolist (pr (available-providers))
      (let ((name (provider-string pr)))
        (setf (ningle:route app (concatenate 'string "/" *login-extension* name)
                     :method :get)
              (lambda (params)
                (declare (ignore params))
                (login-action pr)))
        (setf (ningle:route app (concatenate 'string "/" *login-extension*)
                            :method :get)
              #'login-page)
        (setf (ningle:route
               app (concatenate 'string "/" *callback-extension* name)
               :method :get)
              (lambda (params)
                (callback-action pr params #'logged-in)))))
    (setf (ningle:route app (concatenate 'string "/" *logout-extension*)
                        :method :get)
          #'logout-page)
    app))

(defun component (base-url)
  (lambda (app)
    (let ((lapp (lack.component:to-app (login-app base-url))))
      (lambda (env)
        (let ((extension
               (webhax:under-path-p
                *openid-app-address* (getf env :path-info))))
          (if extension
              (funcall lapp (webhax:repath-clack-env env extension))
              (let ((res (funcall app env)))
                (if (eq 403 (car res))
                    (not-logged-page env res)
                    res))))))))

(defun secrets-from-ubiquitous ()
  (ubiquitous:restore 'openid-connect)
  (let ((providers (loop for (k . v) on *provider-info* by #'cddr collect k)))
    (mapcan (lambda (pr)
              (when (ubiquitous:value pr)
                (list pr
                      (list :client-id (ubiquitous:value pr :client-id)
                            :secret (ubiquitous:value pr :secret)))))
            providers)))

(defun initialize-secrets ()
  (setf *provider-secrets* (secrets-from-ubiquitous)))

(defun login-links ()
  (with-html-output-to-string (s)
    (:div
     (dolist (pr (available-providers))
       (htm (:p (:a :href (make-login-url pr)
                    (format s "~:(~a~)" (provider-string pr)))))))))

(defun login-page (params)
  (awhen (assoc "destination" params :test #'equal)
    (setf *login-destination* (cdr it)))
  (funcall
   (webhax:quick-page
    (webhax:add-part :@title "Login")
    (lambda ()
      (webhax:html-out
        (:h1 "Choose a login provider")
        (str (login-links)))))
   nil))

(defun not-logged-page (env result)
  (setf (gethash :oid-connect-destination (getf env :lack.session))
        (webhax:url-from-env env))
  (list
   (car result)
   (second result)
   (list
    (with-html-output-to-string (s)
      (:html
       (:head (:title "Please Log In"))
       (:body (:h1 "Not logged in")
              (:h2 "Choose a login provider")
              (str (login-links))))))))

;;;FIXME: Not in use. Remove?
(defun logged-in-page ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Logged in"))
     (:body
      (:h1 (str
            (format nil "~a is logged in as ~a"
                    (gethash :username (ningle:context :session))
                    (gethash :display-name (ningle:context :session)))))))))

(defun logout-url ()
  (concatenate 'string *openid-app-address* "/" *logout-extension*))

(defun login-url ()
  (concatenate 'string *openid-app-address* "/" *login-extension*))

(defun logout-page (params)
  (declare (ignore params))
  (logout-action)
  (logged-out)
  (funcall
   (webhax:quick-page
    (webhax:add-part :@title "Logged out")
    (lambda ()
      (webhax:html-out
        (:body (:h1 "Logged out")))))))

(defun logged-in ()
  (let ((uinfo (gethash :oid-connect-userinfo (ningle:context :session))))
    (setf (gethash :username (ningle:context :session))
          (format nil "~a@~a" (aand (assoc :sub uinfo) (cdr it))
                  (string-downcase
                   (gethash :oid-connect-provider (ningle:context :session)))))
    (setf (gethash :display-name (ningle:context :session))
          (or (aand (assoc :preferred--username uinfo) (cdr it))
              (aand (assoc :nickname uinfo) (cdr it))
              (aand (assoc :name uinfo) (cdr it))
              (aand (assoc :given--name uinfo) (cdr it))
              (aand (assoc :email uinfo) (cdr it))))))

(defun logged-out ()
  (remhash :username (ningle:context :session))
  (remhash :display-name (ningle:context :session)))




