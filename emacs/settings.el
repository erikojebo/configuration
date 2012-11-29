(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Set window size
;(add-to-list 'default-frame-alist (cons 'width 120))
;(add-to-list 'default-frame-alist (cons 'height 56))

(defun linux-p ()
  (not (or (eq system-type 'cygwin)
           (eq system-type 'windows-nt))))

(defun windows-p ()
  (eq system-type 'windows-nt))

(defun fullscreen (&optional f)
  (interactive)
  (when (linux-p)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))



;; Remove menubar
(menu-bar-mode -1)

;; Don't show the GNU splash screen
(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn
      (if (windows-p)
          (set-frame-size-according-to-resolution)
        (fullscreen))

      ;; Remove toolbar
      (tool-bar-mode -1)

      ;; Remove scrollbar
      (scroll-bar-mode -1)))

(when (linux-p)
                                        ; Fix copy paste in ubuntu
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

(set-frame-position (selected-frame) 250 65)


;; UTF-8 configuration
(prefer-coding-system 'utf-8)

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

;; Do not spread backups all over the disk.
;; Just save all backup files in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))


;; Calendar
(setq european-calendar-style t)
(setq calendar-week-start-day 1)

;; Parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Delete region
(delete-selection-mode t)
(pending-delete-mode t)
