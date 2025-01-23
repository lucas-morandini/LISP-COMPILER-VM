;;; ============================================================================
;;; Fichier : test-compiler-comp.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour les tests des expressions de comparaison
;;; ============================================================================

(require "compiler/src/compiler.lisp")

(defun test-compile-gt ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-gt '(> 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (null (get-vm-registry vm :R0))))
        (format t "test-compile-gt passed~%")
        )

(defun test-compile-lt ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-lt '(< 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (get-vm-registry vm :R0)))
        (format t "test-compile-lt passed~%")
        )

(defun test-compile-eq ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-eq '(= 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (null (get-vm-registry vm :R0))))
        (format t "test-compile-eq passed~%")
        )

(defun test-compile-neq ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-neq '(!= 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (get-vm-registry vm :R0)))
        (format t "test-compile-neq passed~%")
        )

(defun test-compile-geq ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-geq '(>= 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (null (get-vm-registry vm :R0))))
        (format t "test-compile-geq passed~%")
        )

(defun test-compile-leq ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-leq '(<= 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (get-vm-registry vm :R0)))
        (format t "test-compile-leq passed~%")
        )    

(defun run-tests-compiler-comp ()
    "Lance les tests des expressions arithmetiques"
    (test-compile-gt)
    (test-compile-lt)
    (test-compile-eq)
    (test-compile-neq)
    (test-compile-geq)
    (test-compile-leq)
    )

;;(run-tests-compiler-comp)