;;; highlight-defined.el --- Simplify defined highlighting in Emacs Lisp -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun elisp-highlight-defined ()
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (when (require 'highlight-defined nil t)
		(highlight-defined-mode)))))

;;; highlight-defined.el ends here
