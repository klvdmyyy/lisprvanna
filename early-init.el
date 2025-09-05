;;; early-init.el --- Early Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO: See more emacs optimizations in `DoomEmacs' and `minimal-emacs.d'
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024)))
          500)

(setq-default package-enable-at-startup nil)

(setq-default
 ;; Startup screen
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-buffer-menu t

 ;; Scratch buffer
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 initial-buffer-choice nil

 ;; Other
 inhibit-x-resources nil)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add 'display-startup-screen :override #'ignore)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs loaded in %s." (emacs-init-time))))

(defvar emacs-old-file-name-handler-alist (default-toplevel-value
                                           'file-name-handler-alist))

(defun emacs-respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup.  These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     emacs-old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun emacs-restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   (delete-dups (append file-name-handler-alist
                        emacs-old-file-name-handler-alist))))

(let (file-name-handler-alist)
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  emacs-old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       emacs-old-file-name-handler-alist)

  (advice-add 'command-line-1 :around #'emacs-respect-file-handlers)

  (add-hook 'emacs-startup-hook #'emacs-restore-file-name-handler-alist
            101))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;;; Lisp:

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;;; STP:

(setq-default stp-allow-naive-byte-compile t)
(setq stp-source-directory (expand-file-name "package-source" user-emacs-directory)
      stp-info-file (expand-file-name "../stp-pkg-info.eld" stp-source-directory))

(require 'stp-locked (expand-file-name "subtree-package/stp-locked.el" stp-source-directory))
(require 'stp-bootstrap (expand-file-name "subtree-package/stp-bootstrap.el" stp-source-directory))

;; Set up dependencies for STP itself.
(stp-bootstrap)
;; Add installed packages to the load path.
(stp-update-load-paths)

;;; Commands from subtree-package.el:

(autoload 'stp-list "stp"
  "Open STP package manager list."
  t)

(autoload 'stp-install-command "stp"
  "Install command for STP package manager."
  t)

(provide 'early-init)

;;; early-init.el ends here
