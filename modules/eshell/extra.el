;;; extra.el --- Eshell extra utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun switch-to-prev-buffer-or-eshell (arg)
  (interactive "P")
  (if arg
      (eshell arg)          ; or `project-eshell-or-eshell'
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun project-eshell-or-eshell (&optional arg)
  (interactive "P")
  (if (project-current)
      (project-eshell)
    (eshell arg)))

;;; extra.el ends here
