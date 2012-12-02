(require 'cl)

(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun camelcase-to-underscore-region (&optional start end)
  (interactive "r")
  (let ((safe-start (if start start (mark)))
        (safe-end (if end end (point))))
    (replace-buffer-text 
     safe-start safe-end 
     (camelcase-to-underscore (buffer-substring safe-start safe-end)))))

(defun underscore-to-camelcase-region (&optional start end)
  (interactive "r")
  (let ((safe-start (if start start (mark)))
        (safe-end (if end end (point))))
    (replace-buffer-text 
     safe-start safe-end 
     (underscore-to-camelcase (buffer-substring safe-start safe-end)))))

(defun get-region-text ()
  "Returns the text in the currently active region"
  (buffer-substring (mark) (point)))

(defun replace-region-text (s)
  "Replaces the text in the currently active region with the given string"
  (replace-buffer-text (mark) (point)))

(defun replace-buffer-text (start end s)
  (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert s)))

(defun camelcase-to-underscore (s)
  "Converts a string from the format camelCaseString to camel_case_string
Also works with more complex expressions such as:
var fooBar = new FooBar().calculateSomething() which becomes
var foo_bar = new foo_bar().calculate_something()"
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
                   (not (whitespace-p (get-char prev-index)))
                   (or
                    (lowercase-p (get-char prev-index))
                    (lowercase-p (get-char next-index))))
            (append-char-to-result ?_))
          (append-char-to-result (downcase current-char)))
        (incf char-index))
      (list-to-string result))))

(defun underscore-to-camelcase (s)
  "Converts a string from the format an_underscore_string to anUnderscoreString"
  (let* ((char-index 0)
         (string-length (length s))
         (last-index (1- string-length))
         (result '()))
    (flet ((append-char-to-result (c) (setq result (append-item result c)))
           (get-char (i) (aref s i))
           (letter-p (c) (string-match-p "[a-zA-Z]" (char-to-string c))))
      (while (< char-index string-length)
        (let ((current-char (aref s char-index))
              (next-index (1+ char-index))
              (prev-index (1- char-index)))
          (cond ((and 
                  (/= char-index 0)
                  (letter-p current-char)
                  (equal ?_ (get-char prev-index))
                  (some 'letter-p result))
                 (append-char-to-result (upcase current-char)))
                ((or
                  (not (equal ?_ current-char))
                  (notany 'letter-p result)
                  (= char-index last-index))
                 (append-char-to-result current-char))
                ((and
                  (/= char-index last-index)
                  (not (letter-p (get-char next-index))))
                 (append-char-to-result current-char)))
          (incf char-index)))
      (list-to-string result))))

(defmacro none (p lst)
  (let ((result (mapcar (eval p) (eval lst))))
    `(not ,(cons 'or result))))

(defun append-item (lst x)
  (append lst (list x)))

(defun uppercase-p (x)
  (with-case-sensitive-search
   (to-bool (string-match-p "[A-Z|Ä|Å|Ö]" (to-string x)))))

(defun lowercase-p (x)
  (with-case-sensitive-search
   (to-bool (string-match-p "[a-z|å|ä|ö]" (to-string x)))))

(defun whitespace-p (x)
  (to-bool (string-match-p "^\\s-$" (to-string x))))

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
  "Prints message if the given expression does not evaluate to the expected value"
  (eval-when-compile
    (let ((actual expression))
      `(unless (equal ,expected ,actual)
         (message (format "FAIL: Expected (equal %S %S). Actual value: %S"
                          ,expected 
                          (quote ,expression)
                          ,actual))))))

(assert-equal t (none 'null '(1 2 3)))

(assert-equal t (whitespace-p " "))
(assert-equal nil (whitespace-p "a"))
(assert-equal nil (whitespace-p " a"))
(assert-equal t (whitespace-p "\t"))

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
(assert-equal " foo_bar " (camelcase-to-underscore " FooBar "))

(assert-equal "" (underscore-to-camelcase ""))
(assert-equal "a" (underscore-to-camelcase "a"))
(assert-equal "ab" (underscore-to-camelcase "ab"))
(assert-equal "fooBar" (underscore-to-camelcase "foo_bar"))
(assert-equal "myTsqlGenerator" (underscore-to-camelcase "my_tsql_generator"))
(assert-equal "_fooBar" (underscore-to-camelcase "_foo_bar"))
(assert-equal "__fooBar" (underscore-to-camelcase "__foo_bar"))
(assert-equal "fooBar.bazZap()" (underscore-to-camelcase "fooBar.bazZap()"))
(assert-equal "fooBar" (underscore-to-camelcase "foo_bar"))
(assert-equal " fooBar " (underscore-to-camelcase " foo_bar "))
(assert-equal "fooBar__" (underscore-to-camelcase "foo_bar__"))
