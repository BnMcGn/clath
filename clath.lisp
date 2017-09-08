(in-package :clath)

#|

FIXME: Stuff in this file will need to be rethought when a more general
login manager is developed.

|#

(defparameter *openid-app-address* "/clath")
(defparameter *logout-extension* "logout/")

;;;FIXME: *server-url* can't be dynamically set with let, because functions are not
;;; called from with in the app function, but rather by the ningle app. Therefore
;;; *server-url* is setf instead, so further instances of openid-app will stomp it.
;;; Single use only for now.

(defun login-app (base-url)
  (setf *server-url* base-url)
  ;;FIXME: Doesn't work first time this is called.
  (initialize-secrets)
  (let ((app (make-instance 'ningle:<app>)))
    (dolist (pr (available-providers))
      (let ((name (provider-string pr)))
        (if (uses-north-p pr)
            (progn
              (setf (ningle:route
                     app (concatenate 'string "/" *login-extension-north* name)
                     :method :get)
                    (lambda (params)
                      (declare (ignore params))
                      (login-action-north pr)))
              (setf (ningle:route
                     app
                     (concatenate 'string "/" *callback-extension-north* name)
                     :method :get)
                    (lambda (params)
                      (callback-action-north pr params #'logged-in))))
            (progn
              (setf (ningle:route
                     app (concatenate 'string "/" *login-extension* name)
                     :method :get)
                    (lambda (params)
                      (declare (ignore params))
                      (login-action pr)))
              (setf (ningle:route
                     app (concatenate 'string "/" *callback-extension* name)
                    :method :get)
                    (lambda (params)
                      (callback-action pr params #'logged-in)))))))
    (setf (ningle:route app (concatenate 'string "/" *login-extension*)
                        :method :get)
          #'login-page)
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
  (ubiquitous:restore 'clath)
  (let ((providers (loop for (k . v) on *provider-info* by #'cddr collect k)))
    (mapcan (lambda (pr)
              (when (ubiquitous:value pr)
                (list pr
                      (list* :client-id (ubiquitous:value pr :client-id)
                             :secret (ubiquitous:value pr :secret)
                             (when (ubiquitous:value pr :key)
                               (list :key (ubiquitous:value pr :key)))))))
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
   (webhax-route:quick-page
       (:@title "Login")
     (webhax:html-out
       (:h1 "Choose a login provider")
       (str (login-links))))))

(defun not-logged-page (env result)
  (setf (gethash :clath-destination (getf env :lack.session))
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
  (let ((webhax:*should-login-return* nil))
    (funcall
     (webhax:quick-page
         (:@title "Logged out")
       (webhax:html-out
         (:body (:h1 "Logged out")))))))

(defun logged-in ()
  (let* ((uinfo (gethash :clath-userinfo (ningle:context :session)))
         (uname
          (cdr (assoc-or '(:sub :id :user--id) uinfo))))
    (unless uname
      (alexandria:if-let ((msg (assoc-cdr :message uinfo)))
          (error (format nil "Message from OAuth server: ~a" msg))
          (error "Couldn't find user ID in OAuth return data.")))
    (setf (gethash :username (ningle:context :session))
          (format nil "~a@~a" uname
                  (string-downcase
                   (gethash :clath-provider (ningle:context :session)))))
    (setf (gethash :display-name (ningle:context :session))
          (or (cdr (assoc-or
                 '(:preferred--username :nickname :login :name :display--name
                   :given--name :email)
                 uinfo))
              (format nil "~a ~a"
                      (assoc-cdr :first-name uinfo)
                      (assoc-cdr :last-name uinfo))))))

(defun logged-out ()
  (remhash :username (ningle:context :session))
  (remhash :display-name (ningle:context :session)))




