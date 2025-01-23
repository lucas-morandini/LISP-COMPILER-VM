;;; ============================================================================
;;; Fichier : test-vm-registry.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour handle-move
;;; ============================================================================

(require "../src/instructions/vm-imports.lisp")
(require "../src/utils/vm-imports.lisp")
(require "../src/functional-interface/vm.lisp")

(defun test-handle-move ()
  "Teste le comportement de handle-load sur toutes les possibilités"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; 1) (MOVE :R1 :R0) => :R0 = :R1 = 10
    (set-vm-registry vm :R1 10)
    (handle-move vm '(MOVE :R1 :R0))
    (assert (equal (get-vm-registry vm :R0) 10))
    (format t "Test MOVE :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    ;; 2) (MOVE (:CONST 10) :R0) => :R0 = 10
    (handle-move vm '(MOVE (:CONST 10) :R0))
    (assert (equal (get-vm-registry vm :R0) 10))
    (format t "Test MOVE (:CONST 10) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    ;; 3) (MOVE 10 :R0) => :R0 = 10
    (handle-move vm '(MOVE 10 :R0))
    (assert (equal (get-vm-registry vm :R0) 10))
    (format t "Test MOVE 10 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    (format t "test-handle-load passed~%")))


(defun run-tests-registry ()
    "Lance tous les tests de test-vm-registry"
    (test-handle-move))

;;(run-tests-registry)
