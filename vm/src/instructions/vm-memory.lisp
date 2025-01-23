;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-memory.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition des opérations de gestion de la mémoire
;;                  de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; --- Chargement d'une valeur en mémoire ---
(defun handle-load (vm instruct)
  "Gestion de l'instruction (LOAD <src> <dest>).
   - <src> est TOUJOURS interprété comme une adresse mémoire (directe, registre, indexée).
   - <dest> est TOUJOURS un registre."
  (let ((src  (cadr instruct))    ; 2e élément de la forme (LOAD SRC DEST)
        (dest (caddr instruct)))  ; 3e élément
    (cond
      ;; 1) (LOAD 500 R0) => Charger depuis MEM[500] dans R0
      ((and (integerp src) (keywordp dest))
       (let ((val (get-vm-memory-at vm src)))
         (set-vm-registry vm dest val)))

      ;; 2) (LOAD R1 R0) => Charger depuis MEM[ (valeur de R1) ] dans R0
      ;;    Si R1 = 500, alors on lit MEM[500].
      ((and (keywordp src) (keywordp dest))
       (let* ((addr (get-vm-registry vm src))
              (val  (get-vm-memory-at vm addr)))
         (set-vm-registry vm dest val)))

      ;; 3) (LOAD (+ R1 2) R0) => Charger depuis MEM[ R1+2 ] dans R0
      ;;    Par ex., si R1 = 500, on lit MEM[502].
      ((and (is-offset src) (keywordp dest))
       ;; src est de la forme '(+ R1 2)
       (let* ((reg    (cadr src))   ; R1
              (offset (caddr src))  ; 2
              (addr   (+ (get-vm-registry vm reg) offset))
              (val    (get-vm-memory-at vm addr)))
         (set-vm-registry vm dest val)))

      (t
       (error "Erreur d'argument pour l'instruction LOAD: ~s" instruct)))))


;; --- Stockage d'une valeur dans la mémoire ---
(defun handle-store (vm instruct)
  "Gestion de l'instruction STORE <src> <dest>.
   - instruct doit être de la forme (STORE SRC DEST)."
  (let ((src  (cadr instruct))
        (dest (caddr instruct)))
    (cond
      ;; 1) STORE R1 R0 => on écrit le contenu de R1 à l'adresse contenue dans R0
      ((and (keywordp src) (keywordp dest))
       (let ((src-val  (get-vm-registry vm src))
             (dest-val (get-vm-registry vm dest)))
         (set-vm-memory-at vm dest-val src-val)))

      ;; 2) STORE R1 500 => on écrit le contenu de R1 à l'adresse 500
      ((and (keywordp src) (integerp dest))
       (let ((src-val (get-vm-registry vm src)))
         (set-vm-memory-at vm dest src-val)))

      ;; 3) STORE R1 (+ R0 3) => on écrit le contenu de R1 à l'adresse (R0+3)
      ((and (keywordp src) (is-offset dest))
       ;; dest = '(+ R0 3)
       (let* ((reg     (cadr dest))    ;; R0
              (offset  (caddr dest))   ;; 3
              (addr    (+ (get-vm-registry vm reg) offset))
              (src-val (get-vm-registry vm src)))
         (set-vm-memory-at vm addr src-val)))

      ;; 4) STORE (:CONST 3) R2 => on écrit la valeur 3 à l'adresse contenue dans R2
      ((and (is-const src) (keywordp dest))
       ;; src = '(:CONST 3)
       (let* ((src-val  (cadr src))
              (dest-val (get-vm-registry vm dest)))
         (set-vm-memory-at vm dest-val src-val)))

      ;; 5) STORE (:CONST 3) 500 => on écrit la valeur 3 à l'adresse 500
      ((and (is-const src) (integerp dest))
       (let ((src-val (cadr src)))
         (set-vm-memory-at vm dest src-val)))

      (t
       (error "Erreur d'argument pour l'instruction STORE: ~s" instruct)))))
