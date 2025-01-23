(defun compile-defun (expression)
  "Compile (defun nom (params...) body)."
  (let ((fn-name (cadr expression))
        (params  (caddr expression))
        (body    (cadddr expression)))
    (append
     `((LABEL ,fn-name))
     (compile-generic body)
     '((RET)))))
