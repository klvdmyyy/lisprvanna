;;; appearance.el --- Appearance utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(cl-defun set-font! (font &key (weight 'regular) (height 130))
  "Setup FONT for all frames.

This function just calls `set-face-attribute' in all
possible frames also after creating it.

IMPORTANT: By default `set-face-attribute' can cause issues
in `server-mode'."
  (if (not (daemonp))
      (set-face-attribute 'default nil
			  :font font
			  :weight weight
			  :height height)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
		(select-frame frame)
		(set-face-attribute 'default nil
                                    :font font
                                    :weight weight
                                    :height height)))))

(cl-defun set-theme! (theme)
  "Setup THEME for all frames.

This function just calls `load-theme' in all
possible frames also after creating it.

MAYBE: Use `enable-theme' instead?

IMPORTANT: By default `load-theme' can cause visual issues
in `server-mode'."
  (load-theme theme :no-confirm)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme theme :no-confirm))))

(defmacro with-face (str &rest props)
  "Return STR propertized with PROPS."
  (declare (indent defun))
  `(propertize ,str 'face (list ,@props)))

;;; appearnace.el ends here
