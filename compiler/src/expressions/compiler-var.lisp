(defun compile-setq (sym expression)
  (append
   (compile-generic expression)
   (list (list :SET-VAR (lookup-or-create-var-index sym)))))
