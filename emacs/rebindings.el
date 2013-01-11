
;; Rebindings
(global-set-key (kbd "C-x M-b") 'eval-buffer)
(global-set-key (kbd "C-x M-d") 'eval-defun)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-z") 'zap-to-before-char)

(global-set-key (kbd "C-v") 'scroll-up-6-lines)
(global-set-key (kbd "M-v") 'scroll-down-6-lines)

(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)
(global-set-key (kbd "C-M-+") 'indent-whole-buffer)

(global-set-key (kbd "M-t") 'transpose-words)


;; Quick-jump
(require 'quick-jump)
(global-set-key (kbd "C-c b") 'quick-jump-go-back)
(global-set-key (kbd "C-c f") 'quick-jump-go-forward)
(global-set-key (kbd "C-c <space>") 'quick-jump-push-marker)


;; Make yank use numeric argument to repeat the command
(global-set-key (kbd "C-y") (lambda (n)
                                  (interactive "p")
                                  (when (region-active-p)
                                    (delete-region (region-beginning) (region-end)))
                                  (dotimes (i (abs n)) (yank))))


;; For some reason, the global rebinding of backspace also messes up the
;; backspace binding in the minor mode isearch-mode. So, to fix that,
;; an explicit rebinding of backspace in isearch mode is made here.
(global-set-key (kbd "<backspace>") 'delete-backward-char-or-auto-pair)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)
