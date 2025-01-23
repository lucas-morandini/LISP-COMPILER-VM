;;; ============================================================================
;;; Fichier : test-compiler-arithm.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour les tests des expressions arithmetiques
;;; ============================================================================

(require "compiler/src/compiler.lisp")

(defun test-compile-add ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-add '(+ 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (= (get-vm-registry vm :R0) 3)))
        (format t "test-compile-add passed~%")
        )

(defun test-compile-sub ()
    "Test de la compilation de la soustraction"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-sub '(- 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (= (get-vm-registry vm :R0) -1)))
        (format t "test-compile-sub passed~%")
        )

        
    

(defun run-tests-compiler-arithm ()
    "Lance les tests des expressions arithmetiques"
    (test-compile-add)
    )

;;(run-tests-compiler-arithm)