(defun bool-to-string (b) (if b "t" "nil"))

(defun looking-back-from-current (regexp &optional limit greedy)
  "Like looking-back but from char after point and backwards"
  (save-excursion
    (forward-char 1)
    (looking-back regexp limit greedy)))

(defmacro with-case-sensitive-search (body)
  `(let ((old-case-fold-search case-fold-search)
         (case-fold-search nil)) ;; search case sensitively
     (unwind-protect ,body
       (setq case-fold-search old-case-fold-search))))