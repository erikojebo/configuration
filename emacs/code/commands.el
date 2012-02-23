(defun scroll-down-6-lines ()
  (interactive)
  (scroll-down 6))

(defun scroll-up-6-lines ()
  (interactive)
  (scroll-up 6))


(defun revert-all-buffers ()
  "Refreshes All Open Buffers From Their Respective Files"
  (Interactive)
  (Let* ((List (Buffer-List))
	 (Buffer (Car List)))
    (While buffer
      (when (buffer-file-name buffer)
	(set-buffer buffer)
	(revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))


(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


(defun yank-pop-forwards (arg)
  "Cycle forwards in the kill ring.
Useful when you cycle to far back and don't want to go all the way around."
      (interactive "p")
      (yank-pop (- arg)))


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
