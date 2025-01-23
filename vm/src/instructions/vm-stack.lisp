;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-stack.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Definition des operations de la pile de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-push (vm instruct)
    "Gestion de l'instruction PUSH <src>.
    - <src> est une valeur ou un registre."
    (let ((src (cadr instruct)))
        (cond
            ;; 1) PUSH R0 => empile valeur de R0
            ((keywordp src)
             (let ((val (get-vm-registry vm src)))
                (handle-incr vm '(INCR :R7))
               (handle-store vm `(STORE ,src :R7))))
            ;; 2) PUSH (:CONST 10) => empile valeur 10
            ((and (listp src) (is-const src))
             (let ((val (cadr src)))
                (handle-incr vm '(INCR :R7))
               (handle-store vm `(STORE (:CONST ,val ) :R7))))
            ;; 3) PUSH (+ R0 2) => empile R0 augmenté de 2
            ((is-offset src)
             (let* ((reg (cadr src))
                   (offset (caddr src))
                   (val (+ (get-vm-registry vm reg) offset)))
                (handle-incr vm '(INCR :R7))
               (handle-store vm `(STORE (:CONST ,val ) :R7))))
            (t (error "Erreur d'argument pour l'instruction PUSH: ~s" instruct))))
    )
(defun handle-pop (vm instruct)
    "Gestion de l'instruction POP <dest>.
    - <dest> est TOUJOURS un registre."
    (let ((dest (cadr instruct)))
        (cond
            ;; 1) POP R0 => dépile et stocke dans R0
            ((keywordp dest)
             (let ((val (get-vm-registry vm :R7)))
                (handle-load vm `(LOAD :R7 ,dest))
                (handle-decr vm '(DECR :R7))))
            
            (t (error "Erreur d'argument pour l'instruction POP: ~s" instruct))))
    )


