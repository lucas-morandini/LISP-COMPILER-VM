;;; ============================================================================
;;; Fichier : test-compiler-fonc.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour les tests des expressions de fonction
;;; ============================================================================

(require "compiler/src/compiler.lisp")

(defun test-compile-defun ()
    "Test de la compilation de la definition de fonction"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code  '(defun test (x) (+ x 1)))
          (call '(test 1))
          )
        (load-vm vm (compile-generic code))
        (load-vm vm (compile-generic call))
        ;;(run-vm vm)
        (assert (= (get-vm-registry vm :R0) 2)))
        (format t "test-compile-defun passed~%")
        )

(defun run-tests-compiler-fonc ()
    "Lance les tests des expressions de fonction"
    (test-compile-defun)
    )

;;(run-tests-compiler-fonc)