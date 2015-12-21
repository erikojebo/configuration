(defun cs-extract-autoprop-names-region (&optional start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (message "start: %d, end: %d, point: %d" start end (point))
    ;; We have to keep track of if the match is actually contained inside the
    ;; active region. Only matches in the region should be replaced.
    ;; End has to be updated since the position in the buffer changes when
    ;; we replace the entire auto property match with only the property name
    (while (and 
            (re-search-forward
             "^.*\\b\\([_a-zA-Z]+\\)\\s-*{\\s-get\\s-*;\\s-*set\\s-*;\\s-*}.*$"
             nil t)
            (message "point: %d, match-beginning: %d, match-end: %d, end: %d" (point) (match-beginning 0) (match-end 0) end)
            (<= (match-end 0) end))
      (let* ((total-match-length (- (match-end 0) (match-beginning 0)))
             (property-name-length (- (match-end 1) (match-beginning 1)))
             (deleted-char-count (- total-match-length property-name-length)))
        (goto-char (match-beginning 0))
        (replace-match
         (match-string 1))
        (setq end (- end deleted-char-count)))
      (goto-char (match-end 0)))))
