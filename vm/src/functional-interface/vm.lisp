;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Interface fonctionnelle pour la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Importations ---
(require "src/instructions/vm-imports.lisp")
(require "src/utils/vm-imports.lisp")

;; --- Initialisation de la machine virtuelle ---
(defun init-vm (&key (name "default-name") (memsize 1024) (max-stack-size 256))
  "Initialise une nouvelle instance de machine virtuelle avec la mémoire organisée."
  (let ((new-vm (make-vm :name name :max-stack-size max-stack-size)))
    (setf (vm-memory new-vm) (make-array memsize :initial-element 0))
    (clrhash (vm-registry new-vm))
    (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
      (set-vm-registry new-vm reg 0))
    (clrhash (vm-labels new-vm))
    (set-vm-bp new-vm 64)
    (set-vm-fp new-vm 64)
    (set-vm-sp new-vm 64)
    (set-vm-pc new-vm (+ 64 max-stack-size))
    new-vm))

;; --- Réinitialisation de la machine virtuelle ---
(defun reset-vm (vm &optional (memsize 1024) (max-stack-size 256))
  "Réinitialise une machine virtuelle VM."
  (let ((vp 0)
        (cp (+ 64 (vm-max-stack-size vm)))) ; Début du code après la pile
    (setf (vm-memory vm) (make-array memsize :initial-element 0))
    (clrhash (vm-registry vm))
    (dolist (reg '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7))
      (set-vm-registry vm reg 0))
    (clrhash (vm-labels vm))
    (set-vm-fp vm 64)
    (set-vm-bp vm 64)
    (set-vm-sp vm 64)
    (set-vm-pc vm cp)
    (set-vm-flt vm 0)
    (set-vm-fgt vm 0)
    (set-vm-feq vm 0)
    (set-vm-fnil vm 0)
    (set-vm-halted vm 0)
    (set-vm-vp vm vp)
    (set-vm-cp vm cp)))


;; --- Chargement de code dans la VM ---
(defun load-vm (vm code)
  "Charge le code dans la mémoire de la machine virtuelle VM.
  - code est une liste de listes représentant les instructions."
  (format t "Chargement du code dans la machine virtuelle ~A.~%" (get-vm-name vm))
  (let ((cp (get-vm-cp vm))
        (labels (get-vm-labels vm)) ; Table des étiquettes
        (forward-refs (make-hash-table :test 'equal))) ; Table pour les jump dont le label n'est pas encore défini
    (dolist (instruct code)
      (cond
        ;; 1) Définition d'une étiquette
        ((and (listp instruct) (eq (car instruct) 'LABEL))
            (let ((label-name (cadr instruct)))
            (if (gethash label-name labels)
                (error "Label ~A déjà défini" label-name)
                (progn
                    ;; Définir le label dans la table des labels
                    (set-vm-label vm label-name cp)
                    (set-vm-memory-at vm cp `(LABEL ,label-name))
                    ;; Résoudre les références en avant, s'il y en a
                    (let ((refs (gethash label-name forward-refs)))
                        (when refs
                            ;; Résoudre chaque référence
                            (dolist (ref refs)
                            (let ((instruction (get-vm-memory-at vm ref))) ; Lire l'instruction existante
                                (set-vm-memory-at vm ref 
                                                (list (car instruction) ; Conserver le mot-clé de l'instruction (e.g., JMP)
                                                        cp))))            ; Ajouter l'adresse résolue
                            (remhash label-name forward-refs)))))))
        ;; 2) Instruction 
        (t
         ;; 2.1) Instruction de saut
         (if (and (listp instruct) (is-jump-instruct instruct))
             (let ((jmp-keyword (car instruct))
                   (target (cadr instruct)))
               (cond 
                 ;; a) La cible est un label déjà défini
                 ((get-vm-label vm target)
                  (set-vm-memory-at vm cp (list jmp-keyword (gethash target labels))))
                 ;; b) La cible est un label en attente
                 ((symbolp target)
                  (push cp (gethash target forward-refs (list)))
                  (set-vm-memory-at vm cp (list jmp-keyword 0))) ; Adresse temporaire
                 ;; c) La cible est un nombre
                 ((numberp target)
                  (set-vm-memory-at vm cp (list jmp-keyword target)))
                 (t (error "Cible de saut invalide : ~A" target))))
          ;; 2.2) Instruction std
          (set-vm-memory-at vm cp instruct))))
      ;; On incrémente le code pointer
      (incf cp))
    ;; Vérifie si des jump sont en attente
    (maphash (lambda (key _)
               (warn "Référence non résolue : ~A" key))
             forward-refs)
    ;; Mettre à jour CP dans la VM
    (set-vm-cp vm cp)
    (format t "Fin du chargement du code dans la machione virtuelle.~%")
    ;; Afficher les instructions chargées
    (format t "Instructions chargées :~%")
    (loop for i from 320 to (1- cp) do
        (format t "~A : ~A~%" i (get-vm-memory-at vm i))
    )
    (format t "Fin des instructions chargées.~%")
    ))

;; --- Execution de la machine virtuelle ---
(defun run-vm (vm)
    "Exécute la machine virtuelle VM."
    (format t "Execution de la machine virtuelle ~A~%" (get-vm-name vm))
    (loop
        ;; Condition d'arrêt : la machine est arrêtée
        while (eq (get-vm-halted vm) 0) do
          (let* ((pc (get-vm-pc vm)) ;; lecture du compteur ordinal
                 (instruct (get-vm-memory-at vm pc))) ;; lecture de l'instruction
            (cond
              ;; 1) Instruction HALT
              ((is-halt-instruct instruct)
               (progn
                 (set-vm-halted vm 1)
                 (format t "INSTRUCTION : HALT~%")))
              ;; 2) Instruction valides
              ((is-valid-instruct instruct)
               (handler vm instruct)
               (format t "INSTRUCTION : ~A~%" instruct))
              ;; 3) LABEL : ne rien faire
              ((eq (car instruct) 'LABEL))
              ;; 4) Instruction invalide
              (t
               (error "Instruction invalide : ~A" instruct)))
            ;; 3) Incrémenter le compteur ordinal sauf si modifié par l'instruction JMP
            (unless (is-jump-instruct instruct)
            (set-vm-pc vm (1+ (get-vm-pc vm))))))
        ;; 4) Retourner le contenu de R0 apres l'arrêt
        (format t "Valeur de R0 : ~A~%" (get-vm-registry vm :R0))
        (format t "Fin de l'exécution de la machine virtuelle ~A~%" (get-vm-name vm))
        (get-vm-registry vm :R0)
        )

; ;; --- Application d'une fonction a plusieurs arguments dans la VM ---
; (defun apply-vm (fn vm &rest args))