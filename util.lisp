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
        ((when-let ((val (find itm keywords :test #'string-equal)))
           (setf currkey val))
         t)
        ((and in-list (consp itm)
              (find (car itm) keywords :test #'string-equal))
         (push itm keypairs))
        (t (push itm rest))))
    (when currkey
      (push (list currkey) keypairs))
    (values (nreverse keypairs) (nreverse rest))))
