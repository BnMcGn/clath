(in-package :clath)

(defmacro assoc-cdr (key alist &optional (test '#'eql))
  `(cdr (assoc ,key ,alist :test ,test)))

(defun assoc-or (keys alist)
  "Finds the first key in keys that has a match in alist. Will use equal to match
strings."
  (when keys
    (if-let ((res (assoc (car keys) alist
                                    :test (if (stringp (car keys))
                                              #'equal
                                              #'eql))))
      res
      (assoc-or (cdr keys) alist))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun string-equal-case (a b)
  (or (eq a b) (equal (mkstr a) (mkstr b))))

(defun extract-keywords (keywords alist &key in-list)
  (let ((keypairs nil)
        (rest nil)
        (currkey nil))
    (dolist (itm alist)
      (cond
        (currkey
         (push (cons currkey itm) keypairs)
         (setf currkey nil))
        ((and (or (stringp itm) (symbolp itm))
              (when-let ((val (find itm keywords :test #'string-equal)))
                (setf currkey val)))
         t)
        ((and in-list (consp itm)
              (find (car itm) keywords :test #'string-equal))
         (push itm keypairs))
        (t (push itm rest))))
    (when currkey
      (push (list currkey) keypairs))
    (values (nreverse keypairs) (nreverse rest))))


;;; Utilities borrowed from webhax-core. Webhax is a little heavy as a dependency, so these
;;; are here for now.

(defun under-path-p (path testpath)
  (let ((len (length path)))
    (cond
      ((string= path testpath) "/")
      ((and (< len (length testpath))
            (string= testpath path :end1 len)
            (char= (aref testpath len) #\/))
       (subseq testpath len))
      (t nil))))

(defun repath-clack-env (env newpath)
  (loop for (k v) on env by #'cddr
     collect k
     if (eq :path-info k) collect newpath
     else collect v))

(defun url-from-env (env)
  "Extract the current request url from a clack environment."
  (concatenate 'string
   (format nil "~a://" (string-downcase (mkstr (or (getf env :url-scheme)
                                                   (getf env :uri-scheme)))))
   (getf env :server-name)
   (when-let ((port (getf env :server-port)))
     (unless (= 80 port)
       (format nil ":~d" port)))
   (getf env :request-uri)))
