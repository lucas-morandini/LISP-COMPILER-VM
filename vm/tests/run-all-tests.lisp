;;; ============================================================================
;;; Fichier : run-all-tests.lisp
;;; Description : Lancement de tous les tests pour la VM
;;; ============================================================================

(load "./test-vm-struct.lisp") 
(load "./test-vm-memory.lisp")
(load "./test-vm-registry.lisp")
(load "./test-vm-arithm.lisp")

(format t "=== Lancement des tests de test-vm-struct.lisp ===~%")
(run-tests-struct)
(format t "=== Lancement des tests de test-vm-memory.lisp ===~%")
(run-tests-memory)
(format t "=== Lancement des tests de test-vm-registry.lisp ===~%")
(run-tests-registry)
(format t "=== Lancement des tests de test-vm-arithm.lisp ===~%")
(run-tests-arithm)
(format t "=== Tous les tests ont été exécutés ===~%")
