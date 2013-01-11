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


;; Make isearch exit at the beginning of the match
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


(defun align-repeat (start end regexp)
    "Repeat alignment with respect to the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end 
        (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files."))

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))
