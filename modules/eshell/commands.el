;;; commands.el --- Custom Eshell commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(module! (utils appearance))

(defun eshell/shortened-pwd ()
  "Return the shortened PWD.

~/.config/emacs -> ~/.c/emacs

~/.config/emacs/lisp -> ~/.c/e/lisp"
  (let ((splited (string-split
                  ;; TEMP: Temporary fix because `file-name-directory' sometimes
                  ;; can provide nil value. (for example with "~" abbreviated directory)
                  (or (file-name-directory (abbreviate-file-name (eshell/pwd))) "")
                  "/")))
    (concat
     (string-join
      (seq-map
       (lambda (name)
         (if (<= (length name) 2)
             name
           (if (string-equal (substring name 0 1) ".")
               (substring name 0 2)
             (substring name 0 1))))
       splited)
      "/")
     (file-name-base (abbreviate-file-name
                      (eshell/pwd))))))

(defun eshell/pp-last-status ()
  (autoload nerd-icons-faicon "nerd-icons")
  (let ((status (number-to-string eshell-last-command-status)))
    (if (string-equal status "0")
        (with-face (concat (nerd-icons-faicon "nf-fa-check") " " status)
          :foreground "#63c990"
          :weight 'bold)
      (with-face (concat (nerd-icons-faicon "nf-fa-xmark") " " status)
        :foreground "#c75f5f"
        :weight 'bold))))

;;; commands.el ends here

