;;; ============================================================================
;;; Fichier : test-vm-struct.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour l'initialisation et la réinitialisation de la VM
;;; ============================================================================

(load "../src/vm-struct.lisp")
(load "../src/vm-accessors.lisp")

(defun test-init-vm ()
  "Teste la fonction init-vm"
  (let ((vm (init-vm :name "test-vm" :memsize 2048)))
    ;; Vérification du nom
    (assert (equal (vm-name vm) "test-vm"))
    ;; Vérification de la taille mémoire
    (assert (equal (length (vm-memory vm)) 2048))
    ;; Vérification que les registres R0...R7 sont tous à 0
    (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
      (assert (equal (get-vm-registry vm reg) 0)))
    (format t "test-init-vm passed~%")))

(defun test-reset-vm ()
  "Teste la fonction reset-vm"
  (let ((vm (init-vm :name "test-vm" :memsize 2048)))
    ;; On modifie quelques champs
    (set-vm-fp vm 10)
    (set-vm-flt vm 1)
    (set-vm-fgt vm 1)
    (set-vm-feq vm 1)
    (set-vm-memory-at vm 0 99)

    ;; Appel à reset-vm
    (reset-vm vm 2048)

    ;; Vérification des flags et pointeurs
    (assert (equal (vm-fp vm) 0))
    (assert (equal (vm-flt vm) 0))
    (assert (equal (vm-fgt vm) 0))
    (assert (equal (vm-feq vm) 0))
    (assert (equal (vm-bp vm) 0))
    (assert (equal (vm-sp vm) 0))
    (assert (equal (vm-pc vm) 0))

    ;; Vérification de la mémoire
    (assert (equal (aref (vm-memory vm) 0) 0))

    ;; Vérification que tous les registres sont revenus à 0
    (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
      (assert (equal (get-vm-registry vm reg) 0)))

    (format t "test-reset-vm passed~%")))

(defun run-tests-struct ()
  "Lance tous les tests de test-vm-struct"
  (test-init-vm)
  (test-reset-vm))

;; Exécution automatique des tests :
;;(run-tests-struct)
