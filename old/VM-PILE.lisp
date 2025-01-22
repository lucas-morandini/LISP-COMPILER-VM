;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : VM-PILE.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 21/01/2025
;; Description    : Machine virtuelle à piles
;; Usage          : Charger le fichier dans un interpréteur Lisp et appeler les fonctions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Définition de la structure de la machine virtuelle
(defstruct vm
  name             ;; nom de la machine
  stack            ;; pile de données (liste)
  control-stack    ;; pile de contrôle (liste)
  memory           ;; mémoire (array)
  sp               ;; stack pointer (entier, taille de stack)
  fp               ;; frame pointer (entier ou autre usage)
  pc               ;; program counter (entier)
  instructions     ;; liste d'instructions
  labels           ;; table de hachage label->adresse
  )

;; Création/réinitialisation de la VM
(defun reset-vm (name &optional (memsize 1000))
  "Crée et renvoie une instance de VM avec NAME et une mémoire de MEMSIZE."
  (make-vm
   :name name
   :stack '()                   ;; pile vide
   :control-stack '()           ;; pile de contrôle vide
   :memory (make-array memsize :initial-element 0)
   :sp 0                        ;; pointeurs init
   :fp 0
   :pc 0
   :instructions '()            ;; pas d’instructions au départ
   :labels (make-hash-table)    ;; table de hachage vide
   ))

;; Manipulation de la pile de données

(defun vm-push (vm value)
  "Ajoute un élément sur la pile."
  (push value (vm-stack vm))             ;; push front
  (incf (vm-sp vm))                      ;; sp++
  value)

(defun vm-pop (vm)
  "Retire et retourne le sommet de la pile."
  (assert (> (vm-sp vm) 0) (nil) "POP sur pile vide!")
  (decf (vm-sp vm))
  (pop (vm-stack vm)))                   ;; pop front

(defun vm-peek (vm)
  "Retourne le sommet de la pile sans le retirer."
  (car (vm-stack vm)))

;; Manipulation de la pile de contrôle

(defun vm-push-control (vm value)
  "Ajoute un élément sur la pile de contrôle."
  (push value (vm-control-stack vm)))

(defun vm-pop-control (vm)
  "Retire et retourne le sommet de la pile de contrôle."
  (assert (vm-control-stack vm) (nil) "Pile de contrôle vide!")
  (pop (vm-control-stack vm)))

(defun vm-peek-control (vm)
  "Retourne le sommet de la pile de contrôle sans le retirer."
  (car (vm-control-stack vm)))

;; Accès à la mémoire

(defun vm-load (vm address)
  "Charge une valeur de la mémoire et la place sur la pile."
  (vm-push vm (aref (vm-memory vm) address)))

(defun vm-store (vm address)
  "Prend le sommet de la pile et le stocke dans la mémoire."
  (setf (aref (vm-memory vm) address)
        (vm-pop vm)))

;; Charge un programme dans la VM + enregistre les labels
(defun vm-load-program (vm program)
  "Charge un programme dans la VM et initialise PC=0. 
   Stocke également les adresses des labels dans (vm-labels vm)."
  (setf (vm-instructions vm) program
        (vm-pc vm) 0)
  (clrhash (vm-labels vm))  ;; effacer d'anciens labels éventuels
  (loop for i from 0 below (length program)
        for instr in program
        do (when (eq (car instr) :LABEL)
             ;; on enregistre la position i de ce label
             (setf (gethash (cadr instr) (vm-labels vm)) i))))

;; Boucle d'exécution
(defun vm-execute (vm)
  "Exécute le programme présent dans la VM. 
   Termine à l'instruction :HALT ou si on dépasse les instructions."
  (loop
    (let* ((pc (vm-pc vm))
           (code (vm-instructions vm)))
      (when (or (>= pc (length code)) (< pc 0))
        (return))  ;; on sort si pc hors programme

      (let* ((instr (nth pc code))
             (opcode (car instr))
             (arg (cadr instr)))
        ;; Par défaut on avancera pc de 1 après l'instruction,
        ;; sauf si l'instruction elle-même modifie pc (SKIP, CALL, etc.)
        (let ((did-advance-pc nil)) 

          (case opcode
            (:LIT
             (vm-push vm arg))

            (:VAR
             ;; push memory[arg]
             (vm-push vm (aref (vm-memory vm) arg)))

            (:SET-VAR
             ;; pop -> memory[arg]
             (setf (aref (vm-memory vm) arg) (vm-pop vm)))

            (:STACK
             ;; Réserver arg cases sur la pile
             ;; On pousse arg fois NIL, par ex.
             (dotimes (i arg)
               (vm-push vm nil)))

            (:CALL
             ;; CALL <labelName>
             ;; on sauvegarde (fp) et (pc+1) sur la pile de contrôle
             ;; on déplace pc vers l'adresse du label
             ;; Ex.:  (CALL FOO) => jump -> label FOO
             (vm-push-control vm (vm-fp vm))
             (vm-push-control vm (1+ pc))   ;; next instruction
             ;; ajuster le frame pointer si vous le souhaitez
             ;; (setf (vm-fp vm) ???) 
             ;; on saute
             (setf (vm-pc vm) (gethash arg (vm-labels vm)))
             (setf did-advance-pc t))

             (:CMP
                (let ((a (vm-pop vm))
                        (b (vm-pop vm)))
                    ;; Ici on empile (b - a), à toi de décider si tu préfères pousser le résultat
                    ;; ou le garder dans un registre, etc.
                    (vm-push vm (- b a))))

                ;; Saut si la valeur au sommet de la pile est 0
                (:JEQ
                (let ((arg (cadr instr))
                        (val (vm-pop vm)))
                    (when (zerop val)
                    (incf (vm-pc vm) arg)
                    (setf did-advance-pc t))))

                ;; Saut si la valeur au sommet de la pile n’est pas 0
                (:JNE
                (let ((arg (cadr instr))
                        (val (vm-pop vm)))
                    (when (not (zerop val))
                    (incf (vm-pc vm) arg)
                    (setf did-advance-pc t))))

                ;; Saut si la valeur au sommet de la pile est NIL
                (:JNIL
                (let ((arg (cadr instr))
                        (val (vm-pop vm)))
                    (when (null val)
                    (incf (vm-pc vm) arg)
                    (setf did-advance-pc t))))

                ;; Saut si la valeur au sommet de la pile n’est pas NIL
                (:JTRUE
                (let ((arg (cadr instr))
                        (val (vm-pop vm)))
                    (when val
                    (incf (vm-pc vm) arg)
                    (setf did-advance-pc t))))

            (:RTN
             ;; Retour à l'appelant
             (let ((return-value (vm-pop vm)))
               ;; restore fp
               (setf (vm-fp vm) (vm-pop-control vm))
               ;; restore pc
               (setf (vm-pc vm) (vm-pop-control vm))
               ;; on remet le return-value sur la pile
               (vm-push vm return-value))
             (setf did-advance-pc t))

            (:SKIP
             ;; :SKIP n => pc += n
             (incf (vm-pc vm) arg)
             (setf did-advance-pc t))

            (:SKIPNIL
             ;; si le top de pile est NIL => pc += arg
             ;; on pop le top de pile (souvent on veut le consommer)
             (let ((top (vm-pop vm)))
               (when (null top)
                 (incf (vm-pc vm) arg)
                 (setf did-advance-pc t))))

            (:SKIPTRUE
             ;; si le top de pile n'est pas NIL => pc += arg
             ;; on pop le top
             (let ((top (vm-pop vm)))
               (when top
                 (incf (vm-pc vm) arg)
                 (setf did-advance-pc t))))

            (:ADD
             (vm-push vm (+ (vm-pop vm) (vm-pop vm))))

            (:SUB
             (let ((a (vm-pop vm))
                   (b (vm-pop vm)))
               (vm-push vm (- b a))))

            (:MUL
             (vm-push vm (* (vm-pop vm) (vm-pop vm))))

            (:DIV
             (let ((a (vm-pop vm))
                   (b (vm-pop vm)))
               (assert (not (zerop a)) () "Division par zero!")
               (vm-push vm (/ b a))))

            (:LABEL
             ;; Au runtime: ne rien faire, c'est un no-op
             ;; (On a déjà enregistré le label dans vm-load-program)
             nil)

            (:HALT
             (return))

            (t
             (error "Instruction inconnue: ~A" instr)))

          ;; si l'instruction n'a pas déjà modifié pc, on l'incrémente
          (unless did-advance-pc
            (incf (vm-pc vm))))))))

;; Petite fonction de test
(defun run-test-basic-add ()
  (let ((vm (reset-vm "TestVM")))
    (vm-load-program vm
                     '((:LIT 10)
                       (:LIT 20)
                       (:ADD)
                       (:HALT)))
    (vm-execute vm)
    (assert (= (vm-peek vm) 30) nil "Test addition échoué")
    (format t "Tous les tests sont passés.~%")))

;; Test avancé avec fibonacci
(defun run-test-fibonacci ()
  (let ((vm (reset-vm "Fibonacci")))
    (vm-load-program vm
                     '((:LABEL FIBO) ;; déclaration de l’étiquette
                        (:STACK 0) ;; pas de variables locales
                        (:LIT 1) ;; on empile le litéral
                        (:VAR 1) ;; on empile la valeur de la variable 1 (1ère après FP)
                        (:LIT 2) ;; le nombre d’arguments
                        (:CMP) ;; on compare les deux valeurs
                        (:JNE 2) ;; si elles sont différentes, on saute de 2 instructions
                        (:LIT 1) ;; on empile le litéral 1
                        (:RTN) ;; retour
                        (:VAR 1) ;; on empile la valeur de la variable 1
                        (:LIT 1) ;; on empile le litéral 1
                        (:SUB) ;; on soustrait 1 à la valeur de la variable 1
                        (:LIT 1) ;; le nombre d’arguments
                        (:CALL FIBO) ;; on appelle la fonction FIBO
                        (:VAR 1) ;; on empile la valeur de la variable 1
                        (:LIT 2) ;; on empile le litéral 2
                        (:SUB) ;; on soustrait 2 à la valeur de la variable 1
                        (:LIT 1) ;; le nombre d’arguments
                        (:CALL FIBO) ;; on appelle la fonction FIBO
                        (:ADD) ;; on additionne les deux valeurs
                        (:RTN))) ;; retour (la valeur de retour est déjà sur le sommet de la pile)
    (vm-execute vm)
    (assert (= (vm-peek vm) 8) nil "Test Fibonacci échoué")
    (format t "Tous les tests sont passés.~%")))



;; Lance les tests
(run-test-basic-add)
;;(run-test-fibonacci)