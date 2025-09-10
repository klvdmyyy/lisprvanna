;;; autosave.el --- Auto Save setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(module! (utils lazydo))

(after-init!
 (custom-set-variables
  '(auto-save-default t)
  '(auto-save-interval 20)
  '(auto-save-include-big-deletions t)
  '(auto-save-timeout 5)))

;;; autosave.el ends here
