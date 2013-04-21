
(define-minor-mode gosu-tab-mode
  "Custom mode for smart completion"
  :global t
  :lighter " gosu-tab"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") 'gosu-tab)
            map))

(defun gosu-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (cond
   ((boundp 'ido-cur-item)
    (ido-complete))
   ((minibufferp)
    (unless (minibuffer-complete)
      (hippie-expand nil)))
   (mark-active
    (indent-region (region-beginning)
                   (region-end)))
   ((looking-at "\\_>") ;; end of a symbol?
    (unless (hippie-expand nil) ;; try to hippie expand, otherwise just tab
      (progn
        (gosu-tab-mode -1)
        (let ((binding (key-binding (kbd "TAB"))))
          (if (commandp binding)
              (call-interactively binding)
          (message "No command bound to <tab>")))
        (gosu-tab-mode t))))
    (t (let ((gosu-tab-mode -1))
      (progn
        (gosu-tab-mode -1)
        (let ((binding (key-binding (kbd "TAB"))))
          (if (commandp binding)
              (call-interactively binding)
          (message "No command bound to <tab>")))
        (gosu-tab-mode t))))))

(defun org-cycle-or-expand ()
  (interactive)
  (if (looking-back "^.+\\w\\b")
      (smart-tab 'org-cycle)
    (org-cycle)))

(gosu-tab-mode nil)
