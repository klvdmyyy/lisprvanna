;;; lazydo.el --- Lazy Loading utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defmacro set! (&rest args)
  (pcase args
    ((pred seq-empty-p)
     `())
    (`(,var ,val . ,next)
     (cond ((and (boundp var)
		 ;; For `mode-line-format' like variables.
		 (not (alist-get var (buffer-local-variables))))
	    `(progn
	       (setq ,var ,val)
	       (set! ,@next)))
	   (t `(progn
		 (setq-default ,var ,val)
		 (set! ,@next)))))
    (_ (error "Unexpected arguments: %S" args))))

(defmacro after-init! (&rest body)
  `(add-hook 'after-init-hook
	     (lambda ()
	       ,@body)))

(defmacro before-init! (&rest body)
  `(add-hook 'before-init-hook
	     (lambda ()
	       ,@body)))

(defvar lazydo--after!-counter 0
  "DON'T TOUCH IT!!!")

(defmacro after! (object &rest body)
  "Better alternative to `with-eval-after-load'.

WARN: Function binding can cause issue. Be carefully with collisions.
For example: `dired' as package and `dired' as command.

TODO: `:any' keyword"
  (declare (indent defun))
  (let ((object (eval `(,@object))))
    (cond
     ((not object)
      `(progn ,@body))
     ((symbolp object)
      (pcase object
	((pred fboundp)
	 (let ((fnname (intern (concat "afn" (number-to-string lazydo--after!-counter)))))
	   (setq lazydo--after!-counter (1+ lazydo--after!-counter))
	   `(define-advice ,object
		(:before (&rest _) ,fnname)
	      (advice-remove ',object #',(intern (concat (prin1-to-string object)
							 "@" (prin1-to-string fnname))))
	      ,@body)))
	(_
	 `(with-eval-after-load ',object
	    ,@body))))
     ((listp object)
      `(with-eval-after-load ',(car object)
	 (after! ',(cdr object)
	   ,@body)))
     (t (error "`after!' object must be symbol or list of symbols")))))

;;; lazydo.el ends here
