(in-package :clack-openid-connect)

(defmacro assoc-cdr (key alist &optional (test '#'eql))
  `(cdr (assoc ,key ,alist :test ,test)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun eq-symb-case (a b)
  (or (eq a b) (equal (mkstr a) (mkstr b))))

(defun eq-symb (a b)
  (or (eq-symb-case a b)
      (equal (string-upcase (mkstr a)) (string-upcase (mkstr b)))))

(defun extract-keywords (keywords alist &key in-list)
  (let ((keypairs nil)
        (rest nil)
        (currkey nil))
    (dolist (itm alist)
      (anaphora:acond
        (currkey
         (push (cons currkey itm) keypairs)
         (setf currkey nil))
        ((find itm keywords :test #'eq-symb)
         (setf currkey it))
        ((and in-list (consp itm) (find (car itm) keywords :test #'eq-symb))
         (push itm keypairs))
        (t (push itm rest))))
    (when currkey
      (push (list currkey) keypairs))
    (values (nreverse keypairs) (nreverse rest))))