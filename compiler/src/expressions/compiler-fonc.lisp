(defun compile-defun (expression)
  "Compile (defun nom (params...) body)." ;; to define a variable in the vm : (@ var)
  (let ((fn-name (cadr expression))
        (params  (caddr expression))
        (body    (cadddr expression)))
        (format t "fn-name : ~a~%" fn-name)
        (format t "params : ~a~%" params)
        (format t "body : ~a~%" body)
    (append 
      ;; 1) LABEL de la fonction
      `((LABEL ,fn-name))
      ;; 2) Definition des parametres avec (@ var)
      ; (mapcar (lambda (param)
      ;           `(@ ,param))
      ;         params)
      ;; 3) Compilation du corps de la fonction
      (compile-generic body)
      ;; 4) Retour de la fonction
      `((RTN)))))
  
(defun compile-call (expression)
  "Compile (nom params...)."
  (let ((fn-name (cadr expression))
        (params  (cddr expression)))
    (append
      ;; 1) Push les parametres
      (mapcar (lambda (param)
                (compile-generic param))
              params)
      ;; 2) Appel de la fonction
      '((JSR fn-name)))
      ;; 3) Recuperation du resultat
      `((POP :R0))))
      

