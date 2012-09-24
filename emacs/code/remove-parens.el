(defun remove-parens ()
  (interactive)
  (save-excursion
    (let* ((start (point))
          (char-before (buffer-substring (1- start) start))
          (current-char (buffer-substring start (1+ start))))
      (cond 
       ((or
        (equal char-before ")")
        (equal char-before "]")
        (equal char-before "}"))
        (let ((opening-char (regex-escape (matching-paren-char char-before)))
              (closing-char (regex-escape char-before)))
          (backward-char)
          (delete-char 1)
          (scan-backward-to-paren opening-char closing-char 0)
          (delete-char 1)))
       ((or
        (equal current-char "(")
        (equal current-char "[")
        (equal current-char "{"))
        (let ((opening-char (regex-escape current-char))
              (closing-char (regex-escape (matching-paren-char current-char))))
          (delete-char 1)
          (scan-forward-to-paren opening-char closing-char 0)
          (backward-char)
          (delete-char 1)))))))

; foo (outer (inner) foo) baz
; foo [outer [inner] foo] baz
; foo {outer {inner} foo} baz
; foo {outer (inner) foo} baz

(defun regex-escape (char)
  (if (or
       (equal char "[")
       (equal char "]"))
      (concatenate 'string "\\" char)
    char))

(defun matching-paren-char (paren-string)
  (cond
   ((equal "(" paren-string) ")")
   ((equal ")" paren-string) "(")
   ((equal "[" paren-string) "]")
   ((equal "]" paren-string) "[")
   ((equal "{" paren-string) "}")
   ((equal "}" paren-string) "{")
   (t nil)))

(defun scan-backward-to-paren (opening-char closing-char nesting-level)
  (let ((at-open-paren (looking-back opening-char))
        (at-closing-paren (looking-back closing-char)))
    (cond
     ((and at-open-paren (= nesting-level 0))
      (backward-char)
      (point))
     (at-open-paren
       (backward-char)
       (scan-backward-to-paren opening-char closing-char (1- nesting-level)))
     (at-closing-paren
      (backward-char)
      (scan-backward-to-paren opening-char closing-char (1+ nesting-level)))
     (t
      (backward-char)
      (scan-backward-to-paren opening-char closing-char nesting-level)))))

(defun scan-forward-to-paren (opening-char closing-char nesting-level)
  (let ((at-open-paren (looking-back opening-char))
        (at-closing-paren (looking-back closing-char)))
    (cond
     ((and at-closing-paren (= nesting-level 0))
      (point))
     (at-closing-paren
      (forward-char)
      (scan-forward-to-paren opening-char closing-char (1- nesting-level)))
     (at-open-paren
      (forward-char)
      (scan-forward-to-paren opening-char closing-char (1+ nesting-level)))
     (t
      (forward-char)
      (scan-forward-to-paren opening-char closing-char nesting-level)))))
