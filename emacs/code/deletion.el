(defun zap-to-before-char (arg char)
  "Kill up to BUT NOT including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
       (interactive "p\ncZap to before char: ")
       (if (char-table-p translation-table-for-input)
           (setq char (or (aref translation-table-for-input char) char)))
       (let ((start-point (point))
             (end-point (search-forward (char-to-string char) nil nil arg)))
         (when (> end-point start-point)
           (backward-char)
           (setq end-point (point)))
         (kill-region start-point end-point)))
