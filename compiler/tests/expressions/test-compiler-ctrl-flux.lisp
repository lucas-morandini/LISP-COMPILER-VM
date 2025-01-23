;;; ============================================================================
;;; Fichier : test-compiler-ctrl-flux.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour les tests des expressions de controle de flux
;;; ============================================================================

(require "compiler/src/compiler.lisp")

(defun test-compile-if ()
    "Test de la compilation de l'addition"
    ;; run compiled code in vm
    (let ((vm (init-vm))
          (code (compile-if '(if (> 1 2) 1 2))))
        (load-vm vm code)
        (run-vm vm)
        (assert (= (get-vm-registry vm :R0) 2)))
        (format t "test-compile-if passed~%")
        )

; (defun test-compile-while ()
;     "Test de la compilation de l'addition"
;     ;; run compiled code in vm
;     (let ((vm (init-vm))
;           (code (compile-while '(while (> 1 2) 1))))
;         (load-vm vm code)
;         (run-vm vm)
;         (assert (null (get-vm-registry vm :R0))))
;         (format t "test-compile-while passed~%")
;         )

; (defun test-compile-for ()
;     "Test de la compilation de l'addition"
;     ;; run compiled code in vm
;     (let ((vm (init-vm))
;           (code (compile-for '(for (set i 0) (< i 10) (set i (+ i 1)) 1))))
;         (load-vm vm code)
;         (run-vm vm)
;         (assert (= (get-vm-registry vm :R0) 1)))
;         (format t "test-compile-for passed~%")
;         )

(defun run-tests-compiler-ctrl-flux ()
    "Lance les tests des expressions de controle de flux"
    (test-compile-if)
    ; (test-compile-while)
    ; (test-compile-for)
    )

;;(run-tests-compiler-ctrl-flux)
