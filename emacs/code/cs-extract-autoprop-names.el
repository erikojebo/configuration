(defun cs-extract-autoprop-names ()
  (interactive)
  (let (original-point start end)
    (if (region-active-p)
        (setq
         start (region-beginning)
         end (region-end)
         original-point (point))
      (setq start (point-min) end (point-max)))
    (let ((output (replace-regexp-in-string
                   "^.*\\b\\([_a-zA-Z]+\\)\\s-*{\\s-get\\s-*;\\s-*set\\s-*;\\s-*}.*$"
                   "\\1"
                   (buffer-substring start end))))
      (with-output-to-temp-buffer "*cs-temp*"
          (princ output)))))
