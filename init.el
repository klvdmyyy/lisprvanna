;;; init.el --- Initialization file of my Lisprvanna -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; TODO: Check and Remove unused packages.
;;
;;; Commentary:
;;
;; Navigation
;; ==========
;;
;; Press C-s for jump between headings (`outline-minor-mode').
;;
;; See "lisp/module.el" for more documentation about modulizing
;; in my Emacs configuration. (I thinking about rewriting these configuration)
;;
;; 
;; Package Management
;; ==================
;;
;; Press "C-c p" or "M-x `stp-list'"
;;
;; For more documentation about subtree-package see
;; source code and original author github.
;;
;;
;; Color themes
;; ============
;;
;; Following theme-packages are pre-installed:
;; * doom-themes
;; * spacemacs-theme
;;
;; By default `spacemacs-dark' is used. For me the best themes is
;; modus, spacemacs and a little themes from `doom-themes'.
;;
;; Also you can take a look to following themes:
;; * zenburn
;;
;;; Code:

(custom-set-variables
 '(user-full-name "Klementiev Dmitry")
 '(user-mail-address "klementievd08@yandex.ru"))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(advice-add 'stp-install-command :before 'package-refresh-contents)

;;; Home Directories:

(defconst agenda-directory "~/agenda"
  "Домашняя Agenda директория.")

(defconst notes-directory "~/notes"
  "Домашняя директория с заметками.

Задумывалась как общая, но в основном тут хранятся
`org' файлы (`org-supertag' заметки).")

(defconst projects-directory "~/projects"
  "Домашняя директория с моими проектами.

Хранит всё что так или иначе может относиться к проектам.")

(defconst photo-directory "~/photo"
  "Домашняя директория с фотками.

Хранит в себе не только фото но и:
* Видео
* Прочие медиа-файлы

В целом хранит в себе все мои(наши) фото и видео.")

;; (defconst video-directory "~/video")
;; (defconst media-directory "~/media")

(defconst documents-directory "~/documents"
  "Домашняя директория с документами.")

;; Load modules handler.
(require 'module)

;;; Modules:

(module! (utils))
(module! (utils lazydo))

(module! (safety backups))
(module! (safety autosave))

(module! (eshell setup))
(module! (eshell extra))
(module! (eshell commands))

(module! (elisp highlight-defined))
(elisp-highlight-defined)		; Enable highlighting for defined ELisp symbols.

(module! (core custom))
(setup-custom-file "custom.el")		; FIXME: Can cause issues with `custom-file' and customizations.

;;; Some options:

(custom-set-variables
 '(make-backup-files nil)
 '(default-input-method "russian-computer")
 '(cursor-type '(bar . 2))
 '(cursor-in-non-selected-windows nil)
 
 ;; Disable *Compile-Log* for packages.
 '(byte-compile-log-warning-function #'ignore))

;; Maximized frame by default.
(push '(fullscreen . maximized) default-frame-alist)

(after-init!
 (global-visual-line-mode 1)
 (recentf-mode 1)
 (which-key-mode 1)
 (blink-cursor-mode -1))

;;; GCMH:

(autoload 'gcmh-mode "gcmh")
(after-init!
 (gcmh-mode 1))

;;; Themes and Fonts:

(after-init!
 ;; (require 'doom-themes)
 ;; (set-theme! 'doom-gruvbox)
 ;; (require 'spacemacs-theme)
 ;; (set-theme! 'spacemacs-dark)
 (require 'doom-themes)
 (set-theme! 'doom-one-light)

 ;; Load font: FiraCode Nerd Font
 (set-font! (find-font (font-spec :name "FiraCode Nerd Font"))))

;;; Highlight Todo:

(autoload 'hl-todo-mode "hl-todo" nil t)
(add-hook 'prog-mode-hook 'hl-todo-mode)

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
 (custom-set-variables
  '(completion-styles '(orderless basic))))

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

(after! 'outline
  (autoload 'consult-outline "consult" nil t)
  (bind-keys :map outline-minor-mode-map
	     ("C-s"   . consult-outline)
	     ("C-S-s" . consult-line)))

(after! 'org
  (autoload 'consult-org-heading "consult-org" nil t)
  (bind-keys :map org-mode-map
	     ("C-s"   . consult-org-heading)
	     ("C-S-s" . consult-line)))

;;; Consult GH (Github Intergration):

;; TODO: Configure `consult-gh'

;;; Embark:

(autoload 'embark-act                 "embark" nil t)
(autoload 'embark-dwim                "embark" nil t)
(autoload 'embark-bindings            "embark" nil t)
(autoload 'embark-prefix-help-command "embark")

(setq prefix-help-command #'embark-prefix-help-command)

(bind-keys ("C-."   . embark-act)
	   ("C-;"   . embark-dwim)
	   ("C-h B" . embark-bindings))

(after! 'embark
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(autoload 'embark-collect-mode "embark" nil t)
(autoload 'consult-preview-at-point-mode "consult" nil t)

(after! 'embark-consult
  (add-hook 'embark-collect-mode 'consult-preview-at-point-mode))

;;; Magit:

(autoload 'magit "magit" nil t)
(bind-key "C-x g" 'magit global-map)

;;; Magit todos:

(autoload 'magit-todos-mode "magit-todos" nil t)
(after! 'magit
  (magit-todos-mode 1))

;;; Magit Forge:

;; TODO: Configure `forge'

;;; Git Modes:

(autoload 'gitignore-mode "git-modes" nil t)
(autoload 'gitattributes-mode "git-modes" nil t)
(autoload 'gitconfig-mode "git-modes" nil t)

(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.gitattributes\\'" . gitattributes-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode))

;;; Avy:

(autoload 'avy-goto-char-2 "avy")
(autoload 'avy-goto-word-1 "avy")

(bind-key "C-c ; c" 'avy-goto-char-2 global-map)
(bind-key "C-c ; w" 'avy-goto-word-1 global-map)

;;; Ace Window:

(autoload 'ace-window "ace-window")
(autoload 'ace-swap-window "ace-window")
(autoload 'ace-delete-window "ace-window")

(bind-keys* ("M-o"   . ace-window)
	    ;; TODO: Maybe change this bindings?!
	    ("M-s"   . ace-swap-window)
	    ("C-x 0" . delete-window)
	    ("C-x 1" . delete-other-windows)
	    ("C-x 2" . split-window-vertically)
	    ("C-x 3" . split-window-horizontally))

;;; Eat:

;; NOTE: For now I using `eat' only for eshell.

;;; Eshell:

(autoload 'eat-eshell-mode "eat" nil t)
(autoload 'eat-eshell-visual-command-mode "eat" nil t)

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(defun my-eshell-prompt ()
  (format
   "\n(%s) %s [%s] %s\n$ "
   user-login-name
   (eshell/shortened-pwd)
   (format-time-string "%H:%M:%S")
   (eshell/pp-last-status)))

(custom-set-variables
 '(eshell-prompt-function #'my-eshell-prompt))

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
  (custom-set-variables
   '(corfu-cycle t)
   '(tab-always-indent 'complete))
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

;;; Leetcode:

(custom-set-variables
 '(leetcode-directory "~/leetcode"))

;;; Tree-Sitter:

(custom-set-variables
 '(treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp"))))

(after! 'treesit
  (custom-set-variables
   '(treesit-font-lock-level 4)))

;;; Bash:

(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(after! 'bash-ts-mode
  (setq bash-ts-mode-hook (append bash-ts-mode-hook sh-mode-hook)))

(add-hook 'bash-ts-mode-hook 'eglot-ensure)

;;; C language:

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(after! 'c-ts-mode
  (setq c-ts-mode-hook (append c-ts-mode-hook c-mode-hook)))

(add-hook 'c-ts-mode-hook 'eglot-ensure)

;;; C++ language:

(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(after! 'c++-ts-mode
  (setq c++-ts-mode-hook (append c++-ts-mode-hook c++-mode-hook)))

(add-hook 'c++-ts-mode-hook 'eglot-ensure)

;;; Python:

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(after! 'python-ts-mode
  (setq python-ts-mode-hook (append python-ts-mode-hook python-mode-hook)))

(add-hook 'python-ts-mode-hook 'eglot-ensure)

;;; Java:

;; TODO: Java setup.
;; I would like to learn Java language =)

;;; Org Mode:

(add-hook 'org-mode-hook 'org-indent-mode)

;; TODO: Org Captures

;;; Org Agenda:

(defun get-agenda-files ()
  "Get value for `org-agenda-files' variable."
  (directory-files-recursively agenda-directory
			       "\\`[A-Za-z0-9]*\\.org\\'"))

;; Update `org-agenda-files' every time you open Org Agenda (execute `org-agenda').
(define-advice org-agenda
    (:before (&rest _) update-files)
  (custom-set-variables
   '(org-agenda-files (get-agenda-files))))

(bind-key "a" 'org-agenda mode-specific-map)

;;; Org Roam:

;; TODO: Lazy loading.

(setq org-roam-v2-ack t)

(autoload 'org-roam-db-autosync-mode "org-roam" nil t)

(add-hook 'emacs-startup-hook 'org-roam-db-autosync-mode)

(custom-set-variables
 '(org-roam-directory notes-directory)
 '(org-roam-db-update-on-save t)
 '(org-roam-node-display-template
   (concat "${title:*} "
	   (propertize "${tags:30}" 'face 'org-tag)))
 ;; '(org-roam-dailies-directory "daily/")
 ;; '(org-roam-dailies-capture-templates '())
 '(org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "${slug}.org"
			 "#+title: ${title}
#+author: Klementiev Dmitry
#+email: klementievd08@yandex.ru
#+date: %<%Y-%m-%d>
")
      :unarowwed t))))

(bind-keys :map mode-specific-map
	   ("n f" . org-roam-node-find)
	   ("n n" . org-roam-buffer-toggle)
	   ("n i" . org-roam-node-insert)
	   ("n C" . org-roam-capture)
	   ("r t" . org-roam-tag-add)
	   ("r T" . org-roam-tag-remove))

(provide 'init)

;;; init.el ends here

;;; Local Variables:
;;; eval: (outline-minor-mode 1)
;;; outline-regexp: ";;; "
;;; outline-heading-end-regexp: "\n"
;;; End:
