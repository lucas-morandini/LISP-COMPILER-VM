;;; ============================================================================
;;; Fichier : test-vm-struct.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour l'initialisation et la réinitialisation de la VM
;;; ============================================================================

(defun test-init-vm ()
  "Teste la fonction init-vm"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; Vérification du nom
    (assert (equal (get-vm-name vm) "test-vm"))
    ;; Vérification de la taille mémoire
    (assert (equal (length (get-vm-memory vm)) 1024))
    ;; Vérification des pointeurs
    (assert (equal (get-vm-bp vm) 64)) ; Base pointer
    (assert (equal (get-vm-fp vm) 64)) ; Frame pointer
    (assert (equal (get-vm-sp vm) 64)) ; Stack pointer
    (assert (equal (get-vm-pc vm) 320)) ; Program counter
    ;; Vérification des labels
    (assert (zerop (hash-table-count (get-vm-labels vm))))
    ;; Vérification des flags
    (assert (equal (get-vm-flt vm) 0))
    (assert (equal (get-vm-fgt vm) 0))
    (assert (equal (get-vm-feq vm) 0))
    (assert (equal (get-vm-fnil vm) 0))
    ;; Vérification des tailles maximales
    (assert (equal (get-vm-max-memory-size vm) 1024))
    (assert (equal (get-vm-max-stack-size vm) 256))
    ;; Vérification des pointeurs VP et CP
    (assert (equal (get-vm-vp vm) 0)) ; Variable pointer
    (assert (equal (get-vm-cp vm) 320)) ; Code pointer
    (format t "test-init-vm passed~%")))

(defun test-reset-vm ()
  "Teste la fonction reset-vm"
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; On modifie quelques champs
    (set-vm-fp vm 10)
    (set-vm-flt vm 1)
    (set-vm-fgt vm 1)
    (set-vm-feq vm 1)
    (set-vm-memory-at vm 0 99)
    (set-vm-registry vm :R0 10)
    (set-vm-label vm "label" 10)
    ;; Vérification des modifications
    (assert (equal (get-vm-fp vm) 10))
    (assert (equal (get-vm-flt vm) 1))
    (assert (equal (get-vm-fgt vm) 1))
    (assert (equal (get-vm-feq vm) 1))
    (assert (equal (get-vm-memory-at vm 0) 99))
    (assert (equal (get-vm-registry vm :R0) 10))
    (assert (equal (get-vm-label vm "label") 10))
    (format t "test-reset-vm: champs modifiés~%")

    ;; Appel à reset-vm
    (reset-vm vm 1024 256)

    ;; Vérification après réinitialisation
    ;; Vérification des flags et pointeurs
    (assert (equal (get-vm-fp vm) 64))
    (assert (equal (get-vm-flt vm) 0))
    (assert (equal (get-vm-fgt vm) 0))
    (assert (equal (get-vm-feq vm) 0))
    (assert (equal (get-vm-bp vm) 64))
    (assert (equal (get-vm-sp vm) 64))
    (assert (equal (get-vm-pc vm) 320))
    ;; Vérification de la mémoire
    (assert (equal (get-vm-memory-at vm 0) 0))
    ;; Vérification des labels
    (assert (zerop (hash-table-count (get-vm-labels vm))))
    ;; Vérification des pointeurs VP et CP
    (assert (equal (get-vm-vp vm) 0)) ; Variable pointer
    (assert (equal (get-vm-cp vm) 320)) ; Code pointer
    (format t "test-reset-vm passed~%")))



(defun run-tests-struct ()
  "Lance tous les tests de test-vm-struct"
  (test-init-vm)
  (test-reset-vm))

;; (run-tests-struct)
