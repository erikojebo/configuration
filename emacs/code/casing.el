(require 'cl)
(require 'gosu-text-helpers "~/configuration/emacs/code/text.el")

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
(assert-equal "fooBär" (underscore-to-camelcase "foo_bär"))
