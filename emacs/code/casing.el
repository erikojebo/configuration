(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun to-underscore (s)
  (let* ((char-index 0)
         (string-length (length s))
         (last-index (1- string-length))
         (result '()))
    (while (< char-index string-length)
      (let ((current-char (aref s char-index))
            (next-index (1+ char-index))
            (prev-index (1- char-index)))
        (when
            (and (uppercase-p current-char)
                 (/= char-index 0)
                 (/= char-index last-index)
                 (or
                  (lowercase-p (aref s prev-index))
                  (lowercase-p (aref s next-index))))
          (setq result (append-item result ?_)))
        (setq result (append-item result (downcase current-char))))
      (incf char-index))
    (list-to-string result)))

(defun append-item (lst x)
  (append lst (list x)))

(defun uppercase-p (s)
  (if (integerp s) 
      (uppercase-p (char-to-string s))
    (with-case-sensitive-search
     (if (string-match-p "[A-Z]" s) t nil))))

(defun lowercase-p (s)
  (if (integerp s) 
      (lowercase-p (char-to-string s))
    (with-case-sensitive-search
     (if (string-match-p "[a-z]" s) t nil))))

(assert (equal nil (uppercase-p ?a)))
(assert (equal t (uppercase-p ?A)))
(assert (equal nil (uppercase-p "a")))
(assert (equal t (uppercase-p "A")))

(assert (equal nil (to-underscore-list nil)))
(assert (equal '(?a ?_ ?b) (to-underscore-list '(?a ?B))))

(assert (equal "" (to-underscore "")))
(assert (equal "a" (to-underscore "a")))
(assert (equal "foo_bar" (to-underscore "fooBar")))
(assert (equal "a" (to-underscore "A")))
(assert (equal "ab" (to-underscore "AB")))
(assert (equal "my_tsql_generator" (to-underscore "MyTSQLGenerator")))
(assert (equal "_foo_bar" (to-underscore "_fooBar")))
(assert (equal "foo_bar_" (to-underscore "fooBar_")))
