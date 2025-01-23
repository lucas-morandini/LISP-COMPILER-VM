;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-jumps.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Definition des operations de saut de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-jmp (vm instruct)
    "Gestion de l'instruction JMP <adresse>.
    - <adresse> est un nombre."
    (let ((label (cadr instruct)))
        (cond
            ;; 1) JMP 500 => saute à l'adresse 500            
            ((numberp label)
             (let ((target (+ label 1)))
                (set-vm-pc vm target)))
            ((is-ref label)
             (let ((target (get-vm-registry vm (cadr label))))
                (set-vm-pc vm target)))
            (t (error "Erreur d'argument pour l'instruction JMP: ~s" instruct))))
    )

(defun handle-jsr (vm instruct)
    "Gestion de l'instruction JSR <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond
            ;; 1) JSR 500 => saute à l'adresse 500 et empile PC+1
            ((numberp adresse)
             (let ((pc (+ (get-vm-pc vm) 1)))
                (handle-push vm `(PUSH (:CONST ,pc)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JSR: ~s" instruct))))
    )

(defun handle-rtn (vm instruct)
    "Getion de l'instruction RTN."
    ;; charge SP dans R0, decremente SP, jump à l'adresse contenue dans R0
    (handle-load vm `(LOAD :R7 :R3))
    (handle-decr vm '(DECR :R7))
    (handle-jmp vm `(JMP (:REF :R3)))
    )

(defun handle-cmp (vm instruct)
  "Gestion de l'instruction CMP <a> <b>.
  - <a> et <b> sont des registres ou des constantes."
  (let ((a (cadr instruct))
        (b (caddr instruct)))
    ;; Réinitialiser les flags
    (set-vm-flt vm 0)
    (set-vm-fgt vm 0)
    (set-vm-feq vm 0)
    (cond
      ;; 1) CMP R0 R1 => compare R0 et R1
      ((and (keywordp a) (keywordp b))
       (let ((val-a (get-vm-registry vm a))
             (val-b (get-vm-registry vm b)))
         (cond
           ((= val-a val-b) (set-vm-feq vm 1))
           ((> val-a val-b) (set-vm-fgt vm 1))
           ((< val-a val-b) (set-vm-flt vm 1)))
           ))
      ;; 2) CMP R0 10 => compare R0 et 10
      ((and (keywordp a) (is-const b))
       (let ((val-a (get-vm-registry vm a))
             (val-b (cadr b)))
         (cond
           ((= val-a val-b) (set-vm-feq vm 1))
           ((> val-a val-b) (set-vm-fgt vm 1))
           ((< val-a val-b) (set-vm-flt vm 1)))))
      ;; 3) CMP 10 R0 => compare 10 et R0
      ((and (is-const a) (keywordp b))
       (let ((val-a (cadr a))
             (val-b (get-vm-registry vm b)))
         (cond
           ((= val-a val-b) (set-vm-feq vm 1))
           ((> val-a val-b) (set-vm-fgt vm 1))
           ((< val-a val-b) (set-vm-flt vm 1)))))
           ;; Affichage des flags
      (t (error "Erreur d'argument pour l'instruction CMP: ~s" instruct)))
      ))


(defun handle-jlt (vm instruct)
    "Gestion de l'instruction JLT <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JLT 500 => saute à l'adresse 500 si FLT = 1
            ((numberp adresse)
             (if (= (get-vm-flt vm) 1)
             (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JLT: ~s" instruct)))
        )
    )

(defun handle-jle (vm instruct)
    "Gestion de l'instruction JLE <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JLE 500 => saute à l'adresse 500 si FLT = 1 ou FEQ = 1
            ((numberp adresse)
             (if (or (= (get-vm-flt vm) 1) (= (get-vm-feq vm) 1))
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JLE: ~s" instruct)))
        )
    )
    
(defun handle-jgt (vm instruct)
    "Gestion de l'instruction JGT <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JGT 500 => saute à l'adresse 500 si FGT = 1
            ((numberp adresse)
             (if (= (get-vm-fgt vm) 1)
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JGT: ~s" instruct)))
        )
    )

(defun handle-jge (vm instruct)
    "Gestion de l'instruction JGE <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JGE 500 => saute à l'adresse 500 si FGT = 1 ou FEQ = 1  sinon JMP a pc+1
            ((numberp adresse)
             (if (or (= (get-vm-fgt vm) 1) (= (get-vm-feq vm) 1))
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))
                ))

            (t (error "Erreur d'argument pour l'instruction JGE: ~s" instruct)))
        )
    )

(defun handle-jeq (vm instruct)
    "Gestion de l'instruction JEQ <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JEQ 500 => saute à l'adresse 500 si FEQ = 1
            ((numberp adresse)
             (if (= (get-vm-feq vm) 1)
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JEQ: ~s" instruct)))
        )
    )

(defun handle-jne (vm instruct)
    "Gestion de l'instruction JNE <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JNE 500 => saute à l'adresse 500 si FEQ = 0
            ((numberp adresse)
             (if (= (get-vm-feq vm) 0)
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JNE: ~s" instruct)))
        )
    )

(defun handle-test (vm instruct)
    "Gestion de l'instruction TEST <src>."
    (let ((src (cadr instruct)))
        (let ((val (cond
                ((keywordp src) (get-vm-registry vm src))
                ((is-const src) (cadr src)))))
            (set-vm-fnil vm (if (null val) 1 0)))))

(defun handle-jnil (vm instruct)
    "Gestion de l'instruction JNIL <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JNIL 500 => saute à l'adresse 500 si FNIL = 1
            ((numberp adresse)
             (if (= (get-vm-fnil vm) 1)
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JNIL: ~s" instruct)))
        )
    )

(defun handle-jtrue (vm instruct)
    "Gestion de l'instruction JTRUE <adresse>.
    - <adresse> est un nombre."
    (let ((adresse (cadr instruct)))
        (cond 
            ;; 1) JTRUE 500 => saute à l'adresse 500 si FNIL = 0
            ((numberp adresse)
             (if (= (get-vm-fnil vm) 0)
                (handle-jmp vm `(JMP ,(get-vm-pc vm)))
                (handle-jmp vm `(JMP ,adresse))))
            (t (error "Erreur d'argument pour l'instruction JTRUE: ~s" instruct)))
        )
    )
