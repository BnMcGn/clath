(in-package :clath)

#|

FIXME: Stuff in this file will need to be rethought when a more general
login manager is developed.

|#

(defparameter *clath-app-address* "clath/")
(defparameter *logout-extension* "logout/")

(defun login-app (base-url app-address)
  ;;FIXME: Doesn't work first time this is called.
  (initialize-secrets)
  (let ((app (make-instance 'ningle:<app>)))
    (dolist (pr (available-providers))
      (let ((name (provider-url-string pr)))
        (if (uses-north-p pr)
            (progn
              (setf (ningle:route
                     app (concatenate 'string "/" *login-extension-north* name)
                     :method :get)
                    (lambda (params)
                      (declare (ignore params))
                      (let ((*server-url* base-url)
                            (*clath-app-address* app-address))
                        (login-action-north pr))))
              (setf (ningle:route
                     app
                     (concatenate 'string "/" *callback-extension-north* name)
                     :method :get)
                    (lambda (params)
                      (let ((*server-url* base-url)
                            (*clath-app-address* app-address))
                        (callback-action-north pr params #'logged-in)))))
            (progn
              (setf (ningle:route
                     app (concatenate 'string "/" *login-extension* name)
                     :method :get)
                    (lambda (params)
                      (declare (ignore params))
                      (let ((*server-url* base-url)
                            (*clath-app-address* app-address))
                        (login-action pr))))
              (setf (ningle:route
                     app (concatenate 'string "/" *callback-extension* name)
                    :method :get)
                    (lambda (params)
                      (let ((*server-url* base-url)
                            (*clath-app-address* app-address))
                        (callback-action pr params #'logged-in))))))))
    (setf (ningle:route app (concatenate 'string "/" *login-extension*)
                        :method :get)
          (lambda (params)
            (let ((*server-url* base-url)
                  (*clath-app-address* app-address))
              (login-page params))))
    (setf (ningle:route app (concatenate 'string "/" *logout-extension*)
                        :method :get)
          (lambda (params)
            (let ((*server-url* base-url)
                  (*clath-app-address* app-address))
              (logout-page params))))
    app))

(defun component (base-url &key (extension *clath-app-address*))
  (lambda (app)
    (let* ((server-url (concatenate 'string base-url extension))
           (lapp (lack.component:to-app
                  (login-app server-url extension))))
      (lambda (env)
        (let ((*server-url* server-url)
              (in-app
               (under-path-p
                (concatenate 'string "/" extension) (getf env :path-info))))
          (if in-app
              (funcall lapp (repath-clack-env env in-app))
              (let* ((*clath-app-address* extension)
                     (res (funcall app env)))
                (if (and (consp res) (eql 403 (car res)))
                    (not-logged-page env res)
                    res))))))))

(defun secrets-from-ubiquitous ()
  (ubiquitous:restore 'clath)
  (let ((providers (loop for (k . v) on *provider-info* by #'cddr collect k)))
    (mapcan (lambda (pr)
              (when (and (ubiquitous:value pr) (not (provider-disabled? pr)))
                (list pr
                      (list* :client-id (ubiquitous:value pr :client-id)
                             :secret (ubiquitous:value pr :secret)
                             (when (ubiquitous:value pr :key)
                               (list :key (ubiquitous:value pr :key)))))))
            providers)))

(defun initialize-secrets ()
  (setf *provider-secrets* (secrets-from-ubiquitous))
  t) ;;Don't dump the secrets to the console when called interactively

;;Run initialize-secrets to make an enable or disable come into effect
(defun disable-provider (pr)
  (ubiquitous:restore 'clath)
  (setf (ubiquitous:value pr :disabled) t))

(defun enable-provider (pr)
  (ubiquitous:restore 'clath)
  (ubiquitous:remvalue pr :disabled))

(defun provider-disabled? (pr)
  (ubiquitous:restore 'clath)
  (ubiquitous:value pr :disabled))

(defparameter *provider-icon-size* "25px")
(defparameter *provider-button-css*
  "padding: 1px 10px;
    border: 1px outset buttonborder;
    border-radius: 3px;
    color: buttontext;
    background-color: buttonface;
    text-decoration: none;
    display: flex;
    align-items: center;
    width: 17rem;
    height: 2.5rem;")

(defparameter *provider-container-css*
  " .provider-container svg {
width:25px;
height:25px;
display:inline;
}")

(defun login-links ()
  (with-html-output-to-string (s)
    (:div
     (dolist (pr (available-providers))
       (when-let ((ifile (provider-icon pr)))
         (htm (:div
               :style "margin: 0.5rem;"
               (:a :href (make-login-url pr)
                   :class "provider-container"
                   :style *provider-button-css*
                   (str (alexandria:read-file-into-string ifile))
                   (:span
                    :style "display: flex; justify-content: center; flex-grow: 1;"
                    (str (format nil "Sign in with ~a" (provider-string pr))))))))))))

(defun clath-page-wrapper (title body-func)
  "Redefine this function to customize the look of all Clath pages."
  (with-html-output-to-string (s)
    (:html
     (:head (:title title)
            (:style :type "text/css" (str *provider-container-css*)))
     (:body (str (funcall body-func))))))

(defun clath-login-page ()
  "Redefine this function to customize the login page."
  (clath-page-wrapper "Login"
                (lambda ()
                  (with-html-output-to-string (s)
                    (:h2 "Choose a login provider")
                    (str (login-links))))))

(defun login-page (params)
  "Internal portion of clath-login-page"
  (when-let ((dest (assoc "destination" params :test #'equal)))
    (setf (gethash :clath-destination (ningle:context :session)) (cdr dest)))
  (clath-login-page))

(defun clath-not-logged-page ()
  "Redefine this function to customize the not-logged-in page."
  (clath-page-wrapper "Please Log In"
                      (lambda ()
                        (with-html-output-to-string (s)
                          (:h1 "Not logged in")
                          (:h2 "Choose a login provider")
                          (str (login-links))))))

(defun not-logged-page (env result)
  "Internal portion of clath-not-logged-page"
  (setf (gethash :clath-destination (getf env :lack.session))
        (url-from-env env))
  ;; Result is the 403 page that came back from down-stack. We want to preserve the headers
  ;; while swapping out the page body.
  (list
   (car result)
   (second result)
   (list
    ;; Generated page may have headers. If so, just take the page text.
    (let ((page (clath-not-logged-page)))
      (if (listp page)
          (car (third page))
          page)))))

(defun clath-logout-page ()
  "Redefine this function to customize the logout page."
  (clath-page-wrapper "Logged Out"
                      (lambda ()
                        (with-html-output-to-string (s)
                          (:h1 "Logged out")))))

(defvar *in-logout-page* nil
  "For code that implements login links. Self destination links should not be used on the logout page, lest the user get stuck in an auto-logout loop. This variable will be set when the logout page is being generated.")

(defun logout-page (params)
  (declare (ignore params))
  (logout-action)
  (logged-out)
  (let ((*in-logout-page* t))
    (clath-logout-page)))

(defun logout-url ()
  (concatenate 'string "/" *clath-app-address* *logout-extension*))

(defun login-url ()
  (concatenate 'string "/" *clath-app-address* *login-extension*))


;;FIXME: :username and :display-name extraction are getting really messy.
;; Should split out into different methods for providers.
(defun logged-in ()
  (let* ((uinfo (gethash :clath-userinfo (ningle:context :session)))
         (uname (userinfo-get-user-id t uinfo)))
    (unless uname
      (if-let ((msg (assoc-cdr :message uinfo)))
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




