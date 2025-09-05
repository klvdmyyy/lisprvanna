;;; early-init.el --- Early Initialization file -*- lexical-binding: t; -*-
;;
;;; Commentary:
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

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq stp-source-directory (expand-file-name "package-source" user-emacs-directory)
      stp-info-file (expand-file-name "../stp-pkg-info.eld" stp-source-directory))

(require 'stp-locked (expand-file-name "subtree-package/stp-locked.el" stp-source-directory))
(require 'stp-bootstrap (expand-file-name "subtree-package/stp-bootstrap.el" stp-source-directory))

;; Set up dependencies for STP itself.
(stp-bootstrap)
;; Add installed packages to the load path.
(stp-update-load-paths)

(provide 'early-init)

;;; early-init.el ends here
