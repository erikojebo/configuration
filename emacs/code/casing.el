(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun camelcase-to-underscore (s)
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

(assert (equal "" (camelcase-to-underscore "")))
(assert (equal "a" (camelcase-to-underscore "a")))
(assert (equal "foo_bar" (camelcase-to-underscore "fooBar")))
(assert (equal "a" (camelcase-to-underscore "A")))
(assert (equal "ab" (camelcase-to-underscore "AB")))
(assert (equal "my_tsql_generator" (camelcase-to-underscore "MyTSQLGenerator")))
(assert (equal "_foo_bar" (camelcase-to-underscore "_fooBar")))
(assert (equal "foo_bar_" (camelcase-to-underscore "fooBar_")))
