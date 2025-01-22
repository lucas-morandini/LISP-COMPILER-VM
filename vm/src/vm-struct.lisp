;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-struct.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition de la structure de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Structure de la machine virtuelle ---
(defstruct vm
    (NAME "default-name") ; Nom de la VM
    (REGISTRY (make-hash-table :test 'equal)) ; Registres generaux
    (FP 0) ; Pointeur de frame 
    (BP 0) ; Base pointer
    (SP 0) ; Stack pointer
    (PC 0) ; Program counter
    (FLT 0) ; Flag booleen plus petit
    (FGT 0) ; Flag booleen plus grand
    (FEQ 0) ; Flag booleen egal
    (MEMORY (make-array 1024 :initial-element 0))) ; Memoire

;; --- Initialisation de la machine virtuelle ---
(defun init-vm (&key (name "default-name") (memsize 1024))
  "Initialisation d'une machine virtuelle."
  (let ((new-vm (make-vm :NAME name
                         :REGISTRY (make-hash-table :test 'equal)
                         :MEMORY (make-array memsize :initial-element 0))))
                            (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
                            (set-vm-registry new-vm reg 0))
    new-vm))

;; --- Réinitialisation de la machine virtuelle ---
(defun reset-vm (vm &optional (memsize 1024))
    "Reset d'une machine virtuelle"
    (set-vm-name vm (get-vm-name vm))
    (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
        (set-vm-registry vm reg 0))
    (set-vm-fp vm 0)
    (set-vm-flt vm 0)
    (set-vm-fgt vm 0)
    (set-vm-feq vm 0)
    (set-vm-bp vm 0)
    (set-vm-sp vm 0)
    (set-vm-pc vm 0)
    (set-vm-memory vm (make-array memsize :initial-element 0)))


