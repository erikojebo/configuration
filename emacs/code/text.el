(require 'cl)
(provide 'gosu-text-helpers)

(defun list-to-string (x) (coerce x 'string))
(defun string-to-list (x) (coerce x 'list))

(defun get-region-text ()
  "Returns the text in the currently active region"
  (buffer-substring (mark) (point)))

(defun replace-buffer-text (start end s)
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (insert s)))

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
