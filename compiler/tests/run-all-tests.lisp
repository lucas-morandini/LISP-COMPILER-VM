;;; ============================================================================
;;; Fichier : run-all-tests.lisp
;;; Description : Lancement de tous les tests pour le compilateur
;;; ============================================================================

(require "compiler/tests/expressions/test-compiler-arithm.lisp")
(require "compiler/tests/expressions/test-compiler-comp.lisp")
(require "compiler/tests/expressions/test-compiler-ctrl-flux.lisp")

(format t "=== Lancement des tests de test-compiler-arithm ===~%")
(run-tests-compiler-arithm)
(format t "=== Lancement des tests de test-compiler-comp ===~%")
(run-tests-compiler-comp)
(format t "=== Lancement des tests de test-compiler-ctrl-flux ===~%")
(run-tests-compiler-ctrl-flux)
(format t "=== Tous les tests ont été exécutés ===~%")

