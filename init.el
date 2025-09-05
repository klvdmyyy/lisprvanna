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
