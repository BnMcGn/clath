(in-package :clack-openid-connect)

#|

FIXME: Stuff in this file will need to be rethought when a more general
login manager is developed.

|#

(defvar *openid-app-address* "openid_connect/")
(defparameter *logout-extension* "logout/")

;;;FIXME: *server-url* can't be dynamically set with let, because functions are not
;;; called from with in the app function, but rather by the ningle app. Therefore
;;; *server-url* is setf instead, so further instances of openid-app will stomp it.
;;; Single use only for now.

(defun app (base-url)
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
        (setf (ningle:route app (concatenate 'string "/" *callback-extension* name)
               :method :get)
              (lambda (params)
                (callback-action pr params #'logged-in)))))
    (setf (ningle:route app (concatenate 'string "/" *logout-extension*)
                        :method :get)
          #'logout-page)
    app))

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
                    (format nil "~:(~a~)" (provider-string pr)))))))))

(defun login-page ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Login"))
     (:body (:h1 "Choose a login provider")
            (login-links)))))

;;;FIXME: Not in use. Remove.
(defun logged-in-page ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Logged in"))
     (:body
      (:h1 (str (format nil "~a is logged in"
                      (gethash :username (ningle:context :session)))))))))

(defun logout-url ()
  (concatenate 'string *server-url* *openid-app-address* *logout-extension*))

(defun logout-page ()
  (logout)
  (logged-out)
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Logged out"))
     (:body (:h1 "Logged out")))))

(defun logged-in ()
  (let ((uinfo (gethash :oid-connect-userinfo (ningle:context :session))))
    (print "Userinfo")
    (print uinfo)
    (setf (gethash :username (ningle:context :session))
          (aand (assoc "email" uinfo :test #'equal) (cdr it)))
    (setf (gethash :display-name (ningle:context :session))
          (or (aand (assoc "preferred_username" uinfo :test #'equal) (cdr it))
              (aand (assoc "nickname" uinfo :test #'equal) (cdr it))
              (aand (assoc "given_name" uinfo :test #'equal) (cdr it))
              (aand (assoc "name" uinfo :test #'equal) (cdr it))
              (aand (assoc "email" uinfo :test #'equal) (cdr it))))))

(defun logged-out ()
  (remhash :username (ningle:context :session))
  (remhash :display-name (ningle:context :session)))




