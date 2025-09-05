(defmacro after-init! (&rest body)
  `(add-hook 'after-init-hook
	     (lambda ()
	       ,@body)))
