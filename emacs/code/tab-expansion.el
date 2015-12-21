
(define-minor-mode gosu-tab-mode
  "Custom mode for smart completion"
  :global t
  :lighter " gosu-tab"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") 'gosu-tab)
            (define-key map (kbd "<tab>") 'gosu-tab)
            (define-key map [tab] 'gosu-tab)
            map))

(defun gosu-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)

  ;; Temporarily disables gosu-tab-mode by using a let binding for the mode variable
  ;; and then calls the command bound to TAB (e.g. the binding that would have been
  ;; used if gosu-tab-mode had been disabled)
  (flet ((call-original-key-binding () (let ((gosu-tab-mode nil)
                                             (binding (key-binding (kbd "TAB"))))
                                         (if (commandp binding)
                                             (call-interactively binding)
                                           (message "No command bound to TAB")))))
    (cond
     ;; If in ido mode, let ido do its thing
     ((boundp 'ido-cur-item)
      (ido-complete))

     ((minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil)))

     ;; If the user has selected a region of text, indent the region instead of
     ;; trying to auto-complete anything
     (mark-active
      (indent-region (region-beginning)
                     (region-end)))

     ;; If the cursor is at the end of a word, try to hippie expand it
     ;; If that fails, call the original key binding insteadm
     ((looking-at "\\_>")
      (unless (hippie-expand nil)
        (call-original-key-binding)))

     ;; None of the special conditions apply, so just do what ever TAB would normally
     ;; do in the current mode
    (t (call-original-key-binding)))))

(gosu-tab-mode nil)
