(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun camelcase-to-underscore (s)
  (let* ((char-index 0)
         (string-length (length s))
         (last-index (1- string-length))
         (result '()))
    (flet ((append-char-to-result (c) (setq result (append-item result c)))
           (get-char (i) (aref s i)))
      (while (< char-index string-length)
        (let ((current-char (aref s char-index))
              (next-index (1+ char-index))
              (prev-index (1- char-index)))
          (when
              (and (uppercase-p current-char)
                   (/= char-index 0)
                   (/= char-index last-index)
                   (or
                    (lowercase-p (get-char prev-index))
                    (lowercase-p (get-char next-index))))
            (append-char-to-result ?_))
          (append-char-to-result (downcase current-char)))
        (incf char-index))
      (list-to-string result))))

(defun append-item (lst x)
  (append lst (list x)))

(defun uppercase-p (x)
  (with-case-sensitive-search
   (if (string-match-p "[A-Z]" (to-string x)) t nil)))

(defun lowercase-p (x)
  (with-case-sensitive-search
   (if (string-match-p "[a-z]" (to-string x)) t nil)))

(defun to-string (x)
  "Converts the input to a string in the way that is appropriate
for that type of input."
  (cond ((integerp x)
         (char-to-string x))
        ((listp x)
         (list-to-string x))
        (t x)))

(defun to-bool (x)
  "Converts the input to either t or nil"
  (if x t nil))

(defmacro assert-equal (expected expression)
  (eval-when-compile
    (let ((actual expression))
      `(unless (equal ,expected ,actual)
         (message (format "FAIL: Expected (equal %s %s). Actual value: %s"
                          ,expected 
                          (quote ,expression)
                          ,actual))))))

(assert-equal t (to-bool 1))
(assert-equal nil (to-bool nil))

(assert-equal "a" (to-string ?a))
(assert-equal "a" (to-string "a"))
(assert-equal "abc" (to-string "abc"))
(assert-equal "abc" (to-string '(?a ?b ?c)))

(assert-equal nil (uppercase-p ?a))
(assert-equal t (uppercase-p ?A))
(assert-equal nil (uppercase-p "a"))
(assert-equal t (uppercase-p "A"))

(assert-equal "" (camelcase-to-underscore ""))
(assert-equal "a" (camelcase-to-underscore "a"))
(assert-equal "foo_bar" (camelcase-to-underscore "fooBar"))
(assert-equal "a" (camelcase-to-underscore "A"))
(assert-equal "ab" (camelcase-to-underscore "AB"))
(assert-equal "my_tsql_generator" (camelcase-to-underscore "MyTSQLGenerator"))
(assert-equal "_foo_bar" (camelcase-to-underscore "_fooBar"))
(assert-equal "foo_bar.baz_zap()" (camelcase-to-underscore "fooBar.bazZap()"))
(assert-equal "foo_bar" (camelcase-to-underscore "FooBar"))
