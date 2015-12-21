; Library with common Lisp extensions for Emacs.
(require 'cl)

(require 'gosu-assert "~/configuration/emacs/code/assert.el")


;; Setup package-install

(require 'package)

;; M-x package-list-packages, search for neotree, press 'i', then 'x'

(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(add-to-list 'load-path "~/configuration/emacs/plugins")

(load "~/configuration/emacs/helpers.el")
(load "~/configuration/emacs/code/conversion.el")
(load "~/configuration/emacs/code/searching.el")
(load "~/configuration/emacs/code/lists.el")
(load "~/configuration/emacs/code/casing.el")
(load "~/configuration/emacs/code/commands.el")
(load "~/configuration/emacs/code/tab-expansion.el")
(load "~/configuration/emacs/code/remove-parens.el")
(load "~/configuration/emacs/code/increment-numbers-in-region.el")
(load "~/configuration/emacs/settings.el")
(load "~/configuration/emacs/modes.el")
(load "~/configuration/emacs/rebindings.el")
