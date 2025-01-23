;;; ============================================================================
;;; Fichier : run-all-tests.lisp
;;; Description : Lancement de tous les tests pour la VM
;;; ============================================================================

(load "./utils/test-vm-struct.lisp") 
(load "./utils/test-vm-type-check.lisp")
(load "./instructions/test-vm-memory.lisp")
(load "./instructions/test-vm-registry.lisp")
(load "./instructions/test-vm-arithm.lisp")
(load "./instructions/test-vm-stack.lisp")
(load "./instructions/test-vm-jumps.lisp")
(load "./instructions/test-vm-various.lisp")
(load "./load/test-vm-load.lisp")

(format t "=== Lancement des tests de test-vm-struct.lisp ===~%")
(run-tests-struct)
(format t "=== Lancement des tests de test-vm-type-check.lisp ===~%")
(run-tests-type-check)
(format t "=== Lancement des tests de test-vm-memory.lisp ===~%")
(run-tests-memory)
(format t "=== Lancement des tests de test-vm-registry.lisp ===~%")
(run-tests-registry)
(format t "=== Lancement des tests de test-vm-arithm.lisp ===~%")
(run-tests-arithm)
(format t "=== Lancement des tests de test-vm-stack.lisp ===~%")
(run-tests-stack)
(format t "=== Lancement des tests de test-vm-jumps.lisp ===~%")
(run-tests-jumps)
(format t "=== Lancement des tests de test-vm-various.lisp ===~%")
(run-tests-various)
(format t "=== Lancement des tests de test-vm-load.lisp ===~%")
(run-tests-load)
(format t "=== Tous les tests ont été exécutés ===~%")
