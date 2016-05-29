;; Load the camelCase file
(load "~/configuration/emacs/plugins/camelcase_mode/camelCase-mode")

;;; Start camelCase-mode whenever a file is 'found' (opened)
(add-hook 'find-file-hook '(lambda () (camelCase-mode 1)))


;; Uniquify: replaces the postfixes <2>, <3>, etc for non-unique buffer names with more sensible stuff
;; such as foo/bar.txt and baz/bar.txt
(require 'uniquify)

(setq 
  uniquify-buffer-name-style 'forward
  uniquify-separator "/")


;;; IDO mode
(require 'ido)
(ido-mode t)


;; Electric-pair-mode
(add-hook 'find-file-hook '(lambda () (electric-pair-mode 1)))

;; setting for auto-close brackets for electric-pair-mode regardless of current major mode syntax table
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\' . ?\')
                            (?\{ . ?\})
                            ))

(add-hook 'emacs-lisp-mode-hook 'set-lisp-mode-electric-pairs)
(add-hook 'lisp-mode-hook 'set-lisp-mode-electric-pairs)
                                   
(defun set-lisp-mode-electric-pairs ()
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              )))


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

;; Elixir mode
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

;; Haskell mode
(load "~/configuration/emacs/plugins/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; Typescript mode
(load "~/configuration/emacs/plugins/typescript-mode/typescript-mode.el")
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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
(require 'yasnippet "~/configuration/emacs/plugins/yasnippet/yasnippet.el")
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


;; gosu-tab-mode
(gosu-tab-mode t)


;; js2-mode

;; Load js2-mode for all JavaScript files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(js2r-add-keybindings-with-prefix "C-c C-m")
;; eg. extract function with `C-c C-m ef`.

(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "M-j") #'er/expand-region)
            (local-set-key (kbd "M-J") #'er/contract-region)))
