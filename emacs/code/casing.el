(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun to-underscore (s)
  (list-to-string (to-underscore-list (string-to-list s))))

(defun to-underscore-list (lst)
  (if (null lst)
      nil
    (if (uppercase-p (car lst))
        (append (list ?_ (downcase (car lst))) (to-underscore-list (cdr lst)))
      (cons (car lst) (to-underscore-list (cdr lst))))))

(defun uppercase-p (s)
  (if (integerp s) 
      (uppercase-p (char-to-string s))
    (with-case-sensitive-search
     (if (string-match-p "[A-Z]" s) t nil))))


(assert (equal nil (uppercase-p ?a)))
(assert (equal t (uppercase-p ?A)))
(assert (equal nil (uppercase-p "a")))
(assert (equal t (uppercase-p "A")))



(assert (equal nil (to-underscore-list nil)))
(assert (equal '(?a ?_ ?b) (to-underscore-list '(?a ?B))))

(assert (equal "" (to-underscore "")))
(assert (equal "a" (to-underscore "a")))
(assert (equal "foo_bar" (to-underscore "fooBar")))