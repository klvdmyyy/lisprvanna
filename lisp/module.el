;;; module.el --- Modulize your Emacs configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst modules-dir (expand-file-name "modules/" user-emacs-directory))

(defun load-module (object)
  "Load OBJECT module.

TODO: Use `featurep' and `provide' for loading every module
only once (without repeating)."
  (unless (listp object)
    (error "You must provide list object to `load-module' function"))

  (when (seq-empty-p object)
    (error "You must provide non-empty list object to `load-module' function"))

  (let ((file-name-handler-alist nil)
        (load-suffixes '(".elc" ".el")))
    (load (expand-file-name
           (concat
            modules-dir
            (string-join (seq-map #'prin1-to-string object) "/")
            ".el")
           user-emacs-directory)
          :no-error :no-message :no-suffix :must-suffix)))

(defmacro module! (module)
  "Load MODULE.

Syntax:
[1?SUBMODULES... [MODULE 2?ADDITIONS...]1?...]"
  (unless (listp module)
    (error "You must provide list object to `module!' macro"))

  `(load-module ',module))

(provide 'module)

;;; module.el ends here
