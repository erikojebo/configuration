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