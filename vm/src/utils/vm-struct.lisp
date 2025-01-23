;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-struct.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition de la structure de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Structure de la machine virtuelle ---
(defstruct vm
    (NAME "default-name")                             ; Nom de la VM
    (REGISTRY (make-hash-table :test 'equal))         ; Registres généraux 
    ;; Note sur les registres et certains pointeurs :
    ;; R0 à R2 sont les registres pour les op : init a 0
    ;; R3 est le registre de retour : init a 0
    ;; R4 est le base pointer : init a 64
    ;; R5 est le program counter : init a 320 ou 64 + max-stack-size (cp)
    ;; R6 est le frame pointer : init a 64
    ;; R7 est le stack pointer : init a 64
    (MEMORY (make-array 1024 :initial-element 0))     ; Mémoire principale
    (LABELS (make-hash-table :test 'equal))           ; Table des étiquettes
    (VARS (make-hash-table :test 'equal))        ; Table des variables
    (FUNCTIONS (make-hash-table :test 'equal))        ; Table des fonctions
    (HALTED 0)                                      ; Flag d'arrêt
    (MAX-MEMORY-SIZE 1024)                            ; Taille maximale de la mémoire
    (MAX-STACK-SIZE 256)                              ; Taille maximale de la pile
    (VP 0)                                            ; Var pointer (parcours des variables statiques)
    (CP 320)                                          ; Code pointer (debut de la zone de code ecrivable)
    ;; Flags pour les comparaisons
    (FLT 0)                                           ; Flag booléen "plus petit"
    (FGT 0)                                           ; Flag booléen "plus grand"
    (FEQ 0)                                           ; Flag booléen "égal"
    (FNIL 0)                                          ; Flag booléen "nil"
)                             

