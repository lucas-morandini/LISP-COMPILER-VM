;;; ============================================================================
;;; Fichier : test-vm-various.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour l'initialisation et la réinitialisation de la VM
;;; ============================================================================

(require "../src/instructions/vm-imports.lisp")
(require "../src/utils/vm-imports.lisp")
(require "../src/functional-interface/vm.lisp")

(defun test-nop ()
  "Teste la fonction handle-nop"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    (handle-nop vm '(NOP))
    (format t "test-handle-nop passed~%")))

(defun test-halt ()
    "Teste la fonction handle-halt"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (handle-halt vm '(HALT))
        (assert (get-vm-halted vm))
        (format t "test-handle-halt passed~%")))

(defun run-tests-various ()
    "Lance tous les tests pour les opérations diverses"
    (test-nop)
    (test-halt))

;;(run-tests-various)