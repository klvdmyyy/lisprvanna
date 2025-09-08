
;; Load modules handler.
(require 'module)

;;; Modules:

(module! (utils))
(module! (utils lazydo))

(module! (eshell setup))
(module! (eshell extra))
(module! (eshell commands))

(module! (elisp highlight-defined))
(elisp-highlight-defined)		; Enable highlighting for defined ELisp symbols.

;;; Some options:

(set! make-backup-files nil
      default-input-method "russian-computer"
      cursor-type '(bar . 2)
      cursor-in-non-selected-windows nil)

(after-init!
 (recentf-mode 1)
 (which-key-mode 1)
 (blink-cursor-mode -1))

;;; GCMH:

(autoload 'gcmh-mode "gcmh")
(after-init!
 (gcmh-mode 1))

;;; Themes and Fonts:

(after-init!
 (require 'doom-themes)
 (set-theme! 'doom-gruvbox-light)

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

(autoload 'consult-theme "consult" nil t)

(autoload 'consult-buffer "consult" nil t)
(autoload 'consult-line "consult" nil t)
(autoload 'consult-goto-line "consult" nil t)

(autoload 'consult-imenu "consult-imenu" nil t)
(autoload 'consult-imenu-multi "consult-imenu" nil t)

(bind-keys ([remap switch-to-buffer] . consult-buffer)
	   ("s-B" . consult-buffer)
	   ("C-s" . consult-line)
	   ([remap goto-line] . consult-goto-line)
	   ([remap imenu] . consult-imenu)
	   ("M-g M-i" . consult-imenu-multi))

;;; Magit:

(autoload 'magit "magit")
(bind-key "C-x g" 'magit global-map)

;;; Avy:

(autoload 'avy-goto-char-2 "avy")
(autoload 'avy-goto-word-1 "avy")

(bind-key "C-c ; c" 'avy-goto-char-2 global-map)
(bind-key "C-c ; w" 'avy-goto-word-1 global-map)

;;; Ace Window:

(autoload 'ace-window "ace-window")
(autoload 'ace-swap-window "ace-window")
(autoload 'ace-delete-window "ace-window")

(bind-keys ("M-o"   . ace-window)
	   ("M-s"   . ace-swap-window)
	   ("C-x 0" . delete-window)
	   ("C-x 1" . delete-other-windows)
	   ("C-x 2" . split-window-vertically)
	   ("C-x 3" . split-window-horizontally))

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

(after! 'dired
  (bind-key "C-d" 'dired-gitignore-mode dired-mode-map))
(add-hook 'dired-mode-hook 'dired-gitignore-mode)

;;; Corfu:

;; Example of lazy loading on command
(after! 'self-insert-command
  (require 'corfu))

(after! 'corfu
  (set! corfu-cycle t)
  (set! tab-always-indent 'complete)
  (global-corfu-mode 1)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)

  (bind-keys* :map corfu-map
              ("TAB" . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              :map corfu-popupinfo-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)))

;;; Cape:

(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-history)

;;; Smartparens:

(defconst default-pairs-list
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\}))
  "List of default pairs.")

(defun open-pair-p (char)
  "Return t if CHAR is opening pair."
  (member char (mapcar (lambda (pairs) (car pairs)) default-pairs-list)))

(defun close-pair-p (char)
  "Return t if CHAR is closing pair."
  (member char (mapcar (lambda (pairs) (cdr pairs)) default-pairs-list)))

(defun indent-between-pairs ()
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (interactive)
  (if (and (open-pair-p (char-before))
           (close-pair-p (char-after)))
      (progn
        (newline-and-indent)
        (unless (eq (char-after) '?\n)
          (newline)
          (indent-according-to-mode)
          (forward-line -1)
          (indent-according-to-mode)))
    (newline-and-indent)))

(bind-key "RET" 'indent-between-pairs prog-mode-map)

(autoload 'smartparens-mode "smartparens" nil t)
(autoload 'smartparens-strict-mode "smartparens" nil t)

(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'smartparens-strict-mode)

(after! 'smartparens
  (require 'smartparens-config)
  (bind-keys :map smartparens-mode-map
             ("M-s" . nil)
             ("M-DEL" . sp-backward-unwrap-sexp)
             ("C-<left>" . sp-forward-barf-sexp)
             ("C-<right>" . sp-forward-slurp-sexp)))

;;; Org Mode:

(add-hook 'org-mode-hook 'org-indent-mode)
