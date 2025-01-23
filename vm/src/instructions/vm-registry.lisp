;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-registry.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition des opérations de gestion des registres
;;                  de la machine virtuelle
;; Commentaires   : Move ne peut pas faire d'accès mémoire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-move (vm instruct)
  "Gestion de l'instruction MOVE <src> <dest>.
   - instruct doit être de la forme (MOVE SRC DEST).
   - SRC est une valeur ou un registre.
   - DEST est TOUJOURS un registre."
    (let ((src  (cadr instruct))    ; 2e élément de la forme (MOVE SRC DEST)
            (dest (caddr instruct)))  ; 3e élément
        (cond
        ;; 1) (MOVE R1 R0) => R0 = R1
        ((and (keywordp src) (keywordp dest))
            (let ((src-val (get-vm-registry vm src)))
                (set-vm-registry vm dest src-val)))
        ;; 2) (MOVE (:CONST 10) R0) => R0 = 10
        ((and (is-const src) (keywordp dest))
            (let ((val (cadr src)))
                (set-vm-registry vm dest val)))
        ;; 3) (MOVE 10 R0) => R0 = 10
        ((and (numberp src) (keywordp dest))
            (set-vm-registry vm dest src))
        (t
         (error "Erreur d'argument pour l'instruction MOVE: ~s" instruct)))))


  