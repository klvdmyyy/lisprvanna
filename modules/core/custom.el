;;; custom.el --- Custom file setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(module! (utils lazydo))

(defun setup-custom-file (file)
  (let ((file (expand-file-name file user-emacs-directory)))
    (custom-set-variables `(custom-file ,file))
    (after-init!
     (load custom-file
	   :no-error :no-message
	   :no-suffix :must-suffix))))

;;; custom.el ends here
