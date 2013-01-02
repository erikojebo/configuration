; Library with common Lisp extensions for Emacs.
(require 'cl)

(require 'gosu-assert "~/configuration/emacs/code/assert.el")

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
