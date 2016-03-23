(in-package :clack-openid-connect)

#|

FIXME: Stuff in this file will need to be rethought when a more general
login manager is developed.

|#

(defvar *openid-app-address* "openid_connect/")
(defparameter *logout-extension* "logout/")

(defun app (base-url)
  (let ((app (make-instance 'ningle:<app>))
        (*server-url* base-url))
    (dolist (pr (available-providers))
      (let ((name (provider-string pr)))
        (setf (ningle:route app (print (concatenate 'string "/" *login-extension* name))
                     :method :get)
              (lambda (params)
                (declare (ignore params))
                (login-action pr)))
        (setf (ningle:route app (concatenate 'string "/" *callback-extension* name)
               :method :get)
              (lambda (params)
                (callback-action pr params)
                (logged-in)))))
    (setf (ningle:route app (concatenate 'string "/" *logout-extension*)
                        :method :get)
          #'logout-page)
    app))

(defun secrets-from-ubiquitous ()
  (ubiquitous:restore :openid-connect)
  (let ((providers (loop for (k . v) on *provider-info* by #'cddr collect k)))
    (mapcan (lambda (pr)
              (list pr
                    (list :client-id (ubiquitous:value pr :client-id))
                    (list :secret (ubiquitous:value pr :secret))))
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
  (let ((uinfo (gethash :oid-connect-userinfo *session*)))
    (setf (gethash :username *session*)
          (aand (assoc "email" uinfo :test #'equal) (cdr it)))
    (setf (gethash :display-name *session*)
          (or (aand (assoc "preferred_username" uinfo :test #'equal) (cdr it))
              (aand (assoc "nickname" uinfo :test #'equal) (cdr it))
              (aand (assoc "given_name" uinfo :test #'equal) (cdr it))
              (aand (assoc "name" uinfo :test #'equal) (cdr it))
              (aand (assoc "email" uinfo :test #'equal) (cdr it))))))

(defun logged-out ()
  (remhash :username *session*)
  (remhash :display-name *session*))




