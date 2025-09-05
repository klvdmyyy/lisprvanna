(defsubst install-firacode-nerd ()
  "."
  (let ((name "FiraCode Nerd Font")
        (src "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip")
        (curl (executable-find "curl"))
        (unzip (executable-find "unzip"))
        (font-dir "~/.local/share/fonts/"))
    (unless (file-exists-p font-dir)
      (mkdir font-dir t))

    (start-process-shell-command
     "firacode-nerd-installation"
     "firacode-nerd-installation"
     (string-join
      (list "curl" "-L" "-o" "/tmp/emacs-font-installation.zip" src
            "&&" "unzip" "-o" "/tmp/emacs-font-installation.zip" "-d" font-dir
            "&&" "rm" "-f" "/tmp/emacs-font-installation.zip"
            "&&" "fc-cache" "-fv")
      " "))
    name))
