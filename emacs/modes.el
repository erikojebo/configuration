;; Load the camelCase file
(load "~/configuration/emacs/plugins/camelcase_mode/camelCase-mode")

;;; Start camelCase-mode whenever a file is 'found' (opened)
(add-hook 'find-file-hook '(lambda () (camelCase-mode 1)))


;; Electric-pair-mode
(add-hook 'find-file-hook '(lambda () (electric-pair-mode 1)))


;; Load autohotkey mode
(load "~/configuration/emacs/plugins/ahk_mode/ahk-mode")
(setq ahk-syntax-directory "~/configuration/emacs/plugins/ahk_mode/ahk-syntax/")
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(add-to-list 'auto-mode-alist '("\\.ahkl$" . ahk-mode))
(autoload 'ahk-mode "ahk-mode")


;; Load ruby mode for rakefiles
(add-to-list 'auto-mode-alist '("rakefile$" . ruby-mode))


;; Load org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;; Haskell mode
(load "~/configuration/emacs/plugins/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;;; iswitch
(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


;; YASnippet
(add-to-list 'load-path "~/configuration/emacs/plugins/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/configuration/emacs/plugins/yasnippet/snippets")
(setf yas/global-mode t)

(setq yas/prompt-functions '(yas/dropdown-prompt))


;; Expand region

(add-load-path "~/configuration/emacs/plugins/expand-region.el/")

(require 'expand-region)
(global-set-key (kbd "M-j") 'er/expand-region)
(global-set-key (kbd "M-J") 'er/contract-region)


;; Multiple-cursors
(add-load-path "~/configuration/emacs/plugins/multiple-cursors.el/")
(require 'multiple-cursors)

(define-prefix-command 'c-x-m-map)
(global-set-key (kbd "C-x m") 'c-x-m-map)

(define-key 'c-x-m-map (kbd "a") 'mc/edit-beginnings-of-lines)
(define-key 'c-x-m-map (kbd "e") 'mc/edit-ends-of-lines)

(global-set-key (kbd "M-i") 'mc/mark-next-like-this)
(global-set-key (kbd "M-I") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-I") 'mc/mark-all-like-this)
(global-set-key (kbd "C-;") 'mc/edit-lines)

;; SmartTab

(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

;; Hippie expand

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))


;; nXml-mode
(setq nxml-slash-auto-complete-flag t)
