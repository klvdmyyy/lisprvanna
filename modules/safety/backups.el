;;; backups.el --- Backups setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(module! (utils lazydo))

(after-init!
 (custom-set-variables
  '(make-backup-files t)
  '(backup-directory-alist `((".*" . ,(expand-file-name
				       "backups/" user-emacs-directory))))))

;;; backups.el ends here
