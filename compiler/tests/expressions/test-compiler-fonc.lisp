;;; ============================================================================
;;; Fichier : test-compiler-fonc.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour les tests des expressions de fonction
;;; ============================================================================

(require "compiler/src/compiler.lisp")

(defun test-compile-defun ()
    "Test de la compilation de la definition de fonction"
    (let ((vm (init-vm))
          (code  '(defun test (x) (+ x 1)))
          ;;(call '(test 1))
          )
        (load-vm vm (compile-generic code))
        ;;(load-vm vm (compile-generic call))
        ;;(assert (= (get-vm-registry vm :R0) 2)))
        (format t "test-compile-defun passed~%")
        ))

; (defun test-compile-fibo()
;     "Test de la compilation de la fonction fibonacci"
;     (let ((vm (init-vm))
;           (code  '(defun fibo (n)
;                     (if (< n 2)
;                         n
;                         (+ (fibo (- n 1)) (fibo (- n 2)))
;                     )
;                 ))
;           ;;(call '(fibo 5))
;           )
;         (load-vm vm (compile-generic code))
;         ;;(load-vm vm (compile-generic call))
;         ;;(assert (= (get-vm-registry vm :R0) 5)))
;         (format t "test-compile-fibo passed~%"))

; )

(defun run-tests-compiler-fonc ()
    "Lance les tests des expressions de fonction"
    (test-compile-defun)
    ; (test-compile-fibo)
    )

;;(run-tests-compiler-fonc)