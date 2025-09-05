(require 'module)

(module! (utils appearance))
(module! (utils lazydo))

(set! make-backup-files nil)

(after-init!
 (set-font! (find-font (font-spec :name "FiraCode Nerd Font")))
 (set-theme! 'modus-operandi))

(autoload 'vertico-mode "vertico" nil t)
(if after-init-time
    (vertico-mode 1)
  (after-init! (vertico-mode 1)))

(autoload 'avy-goto-char-2 "avy")

(bind-key "C-'" 'avy-goto-char-2 prog-mode-map)
(bind-key "C-'" 'avy-goto-word-0 text-mode-map)

(autoload 'ace-window "ace-window")
(autoload 'ace-swap-window "ace-window")
(autoload 'ace-delete-window "ace-window")

(bind-key* "M-o M-c" 'ace-window global-map)
(bind-key* "M-o M-s" 'ace-swap-window global-map)
(bind-key* "M-o M-d" 'ace-delete-window global-map)
(bind-key* "M-o M-v" 'split-window-vertically)
(bind-key* "M-o M-h" 'split-window-horizontally)
