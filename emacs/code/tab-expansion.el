(defun smart-tab (&optional default-command)
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  ;;  (let ((default-command (key-binding [tab])))
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
      (funcall (or default-command 'tab-to-tab-stop))))
   (t (funcall (or default-command 'indent-for-tab-command)))))
;;        (call-interactively default-command)))
;;      (t (call-interactively default-command)))))

(defun org-cycle-or-expand ()
  (interactive)
  (if (looking-back "^.+\\w\\b")
      (smart-tab 'org-cycle)
    (org-cycle)))
