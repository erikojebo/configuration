(require 'gosu-assert "~/configuration/code/assert.el")

(defun looking-back-from-current (regexp &optional limit greedy)
  "Like looking-back but from char after point and backwards"
  (save-excursion
    (forward-char 1)
    (looking-back regexp limit greedy)))

(defmacro with-case-sensitive-search (&rest body)
  `(let ((old-case-fold-search case-fold-search)
         (case-fold-search nil)) ;; search case sensitively
     (unwind-protect ,@body
       (setq case-fold-search old-case-fold-search))))

(defun looking-back-from-point-at-p (point regex)
  "Like looking-back, but from a specific point"
  (save-excursion
    (goto-char point)
    (looking-back regex)))

(defun looking-from-point-at-p (point regex)
  "Like looking-at but from a specific point"
  (save-excursion
    (goto-char point)
    (looking-at-p regex)))
