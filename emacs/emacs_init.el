(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set window size
(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 56))

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

(set-frame-position (selected-frame) 250 65)


;;Define 'first' to return the first element of a list.
(defun first (argList)
  "Return the first element of a list"
  (nth 0 argList))

;;Define 'second' to return the second element of a list
(defun second (argList)
  "Return the second element of a list"
  (nth 1 argList))


;; Load the camelCase file
(load "~/configuration/emacs/plugins/camelcase_mode/camelCase-mode")

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

;; Start camelCase-mode whenever a file is 'found' (opened)
(add-hook 'find-file-hook '(lambda () (camelCase-mode 1)))

;; Remove menubar
(menu-bar-mode -1)

;; Remove toolbar
(tool-bar-mode -1)

;; Remove scrollbar
(scroll-bar-mode -1)

;; Don't show the GNU splash screen
(setq inhibit-startup-message t)

;; Make emacs silently delete excessive backup files
(setq delete-old-versions t)

;; Enable transient mode (highlight marked region)
(setq transient-mark-mode t)

;; Alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight unused lines
(setq indicate-empty-lines t)

;; Highlight trailing whitespaces
(setq show-trailing-whitespace t)

;; Move the mouse cursor away when text cursor gets near
(mouse-avoidance-mode 'exile)

; Library with common Lisp extensions for Emacs.
(require 'cl)

;;Allows syntax highlighting to work, among other things
(global-font-lock-mode 1)

;; Allow resizing of the mini-buffer when necessary
(setq resize-minibuffer-mode t)

(setq scroll-step 1)

;; Wrap long lines
(setq auto-fill-mode 1)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Set the fill-column
(setq-default fill-column 78)

;; set highlight color
;(set-face-background 'region "DimGray")

;; And this will enable versioning with default values.
(setq version-control t)

;; Finally do not spread backups all over the disk.
;; Just save all backup files in this directory.w
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

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

;; Uniquify (replace <2>, <3>, etc with more sensible stuff)
(require 'uniquify)

(setq european-calendar-style t)

(setq calendar-week-start-day 1)

(show-paren-mode 1)

(setq show-paren-delay 0)

;; YASnippet
(load "~/configuration/emacs/plugins/yasnippet/dropdown-list")
(load "~/configuration/emacs/plugins/yasnippet/yasnippet")

(yas/initialize)
(yas/load-directory "~/configuration/emacs/plugins/yasnippet/snippets")
(setf yas/global-mode t)

(setq yas/prompt-functions '(yas/dropdown-prompt))


;; Rebindings
(global-set-key (kbd "C-v") 'scroll-up-6-lines)
(global-set-key (kbd "M-v") 'scroll-down-6-lines)
(defun scroll-down-6-lines () 
  (interactive)
  (scroll-down 6))
(defun scroll-up-6-lines ()
  (interactive)
  (scroll-up 6))


(defun Revert-All-Buffers ()
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

(global-set-key (kbd "C-M-+") 'indent-whole-buffer)