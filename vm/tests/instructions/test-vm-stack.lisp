;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : test-vm-stack.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Tests pour handle-push et handle-pop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "src/instructions/vm-imports.lisp")
(require "src/utils/vm-imports.lisp")
(require "src/functional-interface/vm.lisp")

(defun test-handle-push ()
  "Teste le comportement de handle-push sur toutes les possibilités"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; 1) (PUSH :R0) => MEM[SP] = R0 = 10
    (set-vm-registry vm :R0 10)
    (handle-push vm '(PUSH :R0))
    (assert (equal (get-vm-sp vm) 65)) ; SP doit avoir été incrémenté
    (assert (equal (get-vm-memory-at vm (get-vm-sp vm)) 10))
    (format t "Test PUSH :R0 OK, MEM[SP-1] = ~A~%" (get-vm-memory-at vm (get-vm-sp vm)))
    ;; 2) (PUSH (:CONST 10)) => MEM[SP] = 10
    (handle-push vm '(PUSH (:CONST 10)))
    (assert (equal (get-vm-sp vm) 66)) ; SP doit avoir été incrémenté
    (assert (equal (get-vm-memory-at vm (get-vm-sp vm)) 10))
    (format t "Test PUSH (:CONST 10) OK, MEM[SP-1] = ~A~%" (get-vm-memory-at vm (get-vm-sp vm)))
    ;; 3) (PUSH (+ R0 2)) => MEM[SP] = R0 + 2 = 10 + 2 = 12
    (set-vm-registry vm :R0 10)
    (handle-push vm '(PUSH (+ :R0 2)))
    (assert (equal (get-vm-sp vm) 67)) ; SP doit avoir été incrémenté
    (assert (equal (get-vm-memory-at vm(get-vm-sp vm)) 12))
    (format t "Test PUSH (+ :R0 2) OK, MEM[SP-1] = ~A~%" (get-vm-memory-at vm  (get-vm-sp vm)))
    (format t "test-handle-push passed~%")))

(defun test-handle-pop ()
  "Teste le comportement de handle-pop sur toutes les possibilités"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; 1) (POP :R0) => R0 = MEM[SP] = 10
    (set-vm-memory-at vm 64 10) ; Initialiser MEM[SP] à 10
    (set-vm-memory-at vm 65 20) ; Initialiser MEM[SP+1] à 20
    (set-vm-sp vm 65)          ; Initialiser SP à 65
    (handle-pop vm '(POP :R0))
    (assert (equal (get-vm-registry vm :R0) 20)) ; Vérifie que :R0 reçoit la valeur
    (assert (equal (get-vm-sp vm) 64))          ; SP doit avoir été décrémenté
    (format t "Test POP :R0 OK, R0 = ~A~%" (get-vm-registry vm :R0))
    (format t "test-handle-pop passed~%")))

(defun run-tests-stack ()
  "Lance tous les tests de vm-stack.lisp"
  (test-handle-push)
  (test-handle-pop))

;; Exécution automatique des tests :
;;(run-tests-stack)
