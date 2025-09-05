;;; setup.el --- Eshell mode setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(define-minor-mode eshell-mode-setup
  "Set up environment on `eshell-mode' invocation."
  :group 'eshell
  (if eshell-mode-setup
      (progn
        ;; FIXME: eshell throw error at `completion-at-point' with `all-the-icons-completion-mode' enabled.
        ;; This is just a temporary fix which disable it.
        (when (boundp 'all-the-icons-completion-mode)
          (all-the-icons-completion-mode 0))
        (if (and (boundp 'envrc-global-mode) envrc-global-mode)
            (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
          (setenv "PAGER" ""))
        ;; Use `eshell/clear-scrollback' instead of `eshell/clear'
        (eshell/alias "clear" "clear-scrollback")
        ;; (eshell/alias "cl" "clear-scrollback")
        (eshell/alias "x" "exit")
        ;; TODO: Make more convenient FZF (files, grep and etc).
        (eshell/alias "ff" "project-find-file")
        (eshell/alias "fd" "find-dired $PWD \"\"")
        (eshell/alias "rg" "consult-ripgrep")
        (eshell/alias "gg" "consult-git-grep")
        (eshell/alias "l" "ls -al $1")
        (eshell/alias "e" "find-file $1")
        (eshell/alias "ee" "find-file-other-window $1")
        (eshell/alias "d" "dired $1")
        (eshell/alias "gd" "magit-diff-unstaged")
        ;; (local-unset-key 'eshell/clear)
        )
    (when (boundp 'all-the-icons-completion-mode)
      (all-the-icons-completion-mode 1))))

;;; setup.el ends here
