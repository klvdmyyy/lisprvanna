
;; Load modules handler.
(require 'module)

;;; Modules:

(module! (utils appearance))
(module! (utils lazydo))

(module! (eshell setup))
(module! (eshell extra))
(module! (eshell commands))

;;; Some options:

(set! make-backup-files nil)

;;; Themes and Fonts:

(after-init!
 ;; Nano dark is so interested theme.
 ;;
 ;; With some customizations it can be one of the best
 ;; dark theme which I ever seen.
 (require 'nano-theme)
 (set-theme! 'nano-dark)

 ;; Load font: FiraCode Nerd Font
 (set-font! (find-font (font-spec :name "FiraCode Nerd Font"))))

;;; Vertico:

(autoload 'vertico-mode "vertico" nil t)
(if after-init-time
    (vertico-mode 1)
  (after-init! (vertico-mode 1)))

;;; Marginalia:

(autoload 'marginalia-mode "marginalia" nil t)
(after-init!
 (marginalia-mode 1))

;;; Orderless:

(after-init!
 (require 'orderless)
 (set! completion-styles '(orderless basic)))

;;; Consult:

(autoload 'consult-buffer "consult")
(autoload 'consult-line "consult")
(autoload 'consult-goto-line "consult")
(bind-keys ([remap switch-to-buffer] . consult-buffer)
	   ("s-B" . consult-buffer)
	   ("C-s" . consult-line)
	   ([remap goto-line] . consult-goto-line))

;;; Magit:

(autoload 'magit "magit")
(bind-key "C-x g" 'magit global-map)

;;; Avy:

(autoload 'avy-goto-char-2 "avy")
(autoload 'avy-goto-word-0 "avy")

(bind-key "C-'" 'avy-goto-char-2 prog-mode-map)
(bind-key "C-'" 'avy-goto-word-0 text-mode-map)

;;; Ace Window:

(autoload 'ace-window "ace-window")
(autoload 'ace-swap-window "ace-window")
(autoload 'ace-delete-window "ace-window")

(bind-key* "M-o M-c" 'ace-window global-map)
(bind-key* "M-o M-s" 'ace-swap-window global-map)
(bind-key* "M-o M-d" 'ace-delete-window global-map)
(bind-key* "M-o M-v" 'split-window-vertically)
(bind-key* "M-o M-h" 'split-window-horizontally)

;;; Eshell:

(defun my-eshell-prompt ()
  (format
   "\n(%s) %s [%s] %s\n$ "
   user-login-name
   (eshell/shortened-pwd)
   (format-time-string "%H:%M:%S")
   (eshell/pp-last-status)))

(set! eshell-prompt-function #'my-eshell-prompt)

(bind-key "s-e" 'project-eshell-or-eshell)

(after! 'eshell
  (require 'em-alias)
  (require 'em-hist)

  (add-hook 'eshell-mode-hook 'eshell-mode-setup)

  (bind-key "s-e" 'switch-to-prev-buffer-or-eshell eshell-mode-map)
  (autoload 'consult-history "consult")
  (bind-key "M-r" 'consult-history eshell-hist-mode-map))

;;; Dired Gitignore:

(autoload 'dired-gitignore-mode "dired-gitignore")

(bind-key "C-d" 'dired-gitignore-mode dired-mode-map)
(add-hook 'dired-mode-hook 'dired-gitignore-mode)

;;; Corfu:

;; Example of lazy loading on command
(after! 'self-insert-command
  (require 'corfu))

(after! 'corfu
  (setq corfu-cycle t)
  (setq tab-always-indent 'complete)
  (global-corfu-mode 1)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)

  (bind-keys* :map corfu-map
              ("TAB" . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              :map corfu-popupinfo-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)))
