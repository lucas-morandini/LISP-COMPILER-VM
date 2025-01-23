;;; ============================================================================
;;; Fichier : test-vm-memory.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour handle-load et handle-store
;;; ============================================================================

(require "src/instructions/vm-imports.lisp")
(require "src/utils/vm-imports.lisp")
(require "src/functional-interface/vm.lisp")

(defun test-handle-load ()
  "Teste le comportement de handle-load sur toutes les possibilités"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; :R1 = 10, MEM[10] = 20
    (set-vm-registry vm :R1 10)
    (set-vm-memory-at vm 10 20)

    ;; (LOAD :R1 :R0) => :R0 = MEM[R1] = MEM[10] = 20
    (handle-load vm '(LOAD :R1 :R0))
    (assert (equal (get-vm-registry vm :R0) 20))
    (format t "Test LOAD :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    ;; (LOAD 10 :R0) => :R0 = MEM[10] = 20
    (set-vm-registry vm :R0 0)  ;; Remise à zéro pour vérifier le changement
    (handle-load vm '(LOAD 10 :R0))
    (assert (equal (get-vm-registry vm :R0) 20))
    (format t "Test LOAD 10 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    ;; (LOAD (+ :R1 0) :R0) => :R0 = MEM[(R1 + 0)] = MEM[10] = 20
    (set-vm-registry vm :R0 0)
    (handle-load vm '(LOAD (+ :R1 0) :R0))
    (assert (equal (get-vm-registry vm :R0) 20))
    (format t "Test LOAD (+ :R1 0) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

    (format t "test-handle-load passed~%")))


(defun test-handle-store ()
  "Teste le comportement de handle-store sur toutes les possibilités"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; 1) (STORE :R1 :R0) => MEM[R0] = :R1
    (set-vm-registry vm :R1 10)  ; Valeur à écrire
    (set-vm-registry vm :R0 20)  ; Adresse où écrire
    (handle-store vm '(STORE :R1 :R0))
    (assert (equal (get-vm-memory-at vm 20) 10))
    (format t "Test STORE :R1 :R0 OK, MEM[20] = ~A~%" (get-vm-memory-at vm 20))

    ;; 2) (STORE :R1 20) => MEM[20] = :R1
    (set-vm-registry vm :R1 10)
    (handle-store vm '(STORE :R1 20))
    (assert (equal (get-vm-memory-at vm 20) 10))
    (format t "Test STORE :R1 20 OK, MEM[20] = ~A~%" (get-vm-memory-at vm 20))

    ;; 3) (STORE :R1 (+ :R0 0)) => MEM[R0 + 0] = :R1
    (set-vm-registry vm :R1 10)
    (handle-store vm '(STORE :R1 (+ :R0 0)))
    (assert (equal (get-vm-memory-at vm 20) 10))
    (format t "Test STORE :R1 (+ :R0 0) OK, MEM[20] = ~A~%" (get-vm-memory-at vm 20))

    ;; 4) (STORE (:CONST 10) R1) => MEM[R1] = 10
    (set-vm-registry vm :R1 20)
    (handle-store vm '(STORE (:CONST 10) :R1))
    (assert (equal (get-vm-memory-at vm 20) 10))
    (format t "Test STORE (:CONST 10) :R1 OK, MEM[20] = ~A~%" (get-vm-memory-at vm 20))

    ;; 5) (STORE (:CONST 10) 20) => MEM[20] = 10
    (handle-store vm '(STORE (:CONST 10) 20))
    (assert (equal (get-vm-memory-at vm 20) 10))
    (format t "Test STORE (:CONST 10) 20 OK, MEM[20] = ~A~%" (get-vm-memory-at vm 20))


    (format t "test-handle-store passed~%")))

(defun run-tests-memory ()
  "Lance tous les tests."
  (test-handle-load)
  (test-handle-store))

;;; Exécution des tests
;;(run-tests-memory)
