;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-arithm.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition des opérations arithmétiques
;;                  de la machine virtuelle (ADD, SUB, MUL, DIV, INCR, DECR)
;; Commentaires   : Dans toutes les instructions <dest> est un registre et 
;;                  <src> est soit un registre en mode normal ou indexé,
;;                  soit une valeur en mode direct. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-add (vm instruct)
  "Gestion de l'instruction ADD <src> <dest>.
   - instruct doit être de la forme (ADD SRC DEST).
   - SRC est une valeur ou un registre.
   - DEST est TOUJOURS un registre."
    (let ((src  (cadr instruct))    ; 2e élément de la forme (ADD SRC DEST)
            (dest (caddr instruct)))  ; 3e élément
        (cond
        ;; 1) (ADD R1 R0) => R0 = R0 + R1
        ((and (keywordp src) (keywordp dest))
            (let ((src-val (get-vm-registry vm src))
                  (dest-val (get-vm-registry vm dest)))
                (set-vm-registry vm dest (+ dest-val src-val))))
        ;; 2) (ADD (:CONST 10) R0) => R0 = R0 + 10
        ((and (listp src) (keywordp dest))
            (let ((val (cadr src))
                  (dest-val (get-vm-registry vm dest)))
                (set-vm-registry vm dest (+ dest-val val))))
        (t
         (error "Erreur d'argument pour l'instruction ADD: ~s" instruct)))))

(defun handle-sub (vm instruct)
    "Gestion de l'instruction SUB <src> <dest>.
     - instruct doit être de la forme (SUB SRC DEST).
     - SRC est une valeur ou un registre.
     - DEST est TOUJOURS un registre."
        (let ((src  (cadr instruct))    ; 2e élément de la forme (SUB SRC DEST)
                (dest (caddr instruct)))  ; 3e élément
            (cond
            ;; 1) (SUB R1 R0) => R0 = R0 - R1
            ((and (keywordp src) (keywordp dest))
                (let ((src-val (get-vm-registry vm src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (- dest-val src-val))))
            ;; 2) (SUB (:CONST 10) R0) => R0 = R0 - 10
            ((and (listp src) (keywordp dest))
                (let ((val (cadr src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (- dest-val val))))
            (t
             (error "Erreur d'argument pour l'instruction SUB: ~s" instruct)))))

(defun handle-mul (vm instruct)
    "Gestion de l'instruction MUL <src> <dest>.
     - instruct doit être de la forme (MUL SRC DEST).
     - SRC est une valeur ou un registre.
     - DEST est TOUJOURS un registre."
        (let ((src  (cadr instruct))    ; 2e élément de la forme (MUL SRC DEST)
                (dest (caddr instruct)))  ; 3e élément
            (cond
            ;; 1) (MUL R1 R0) => R0 = R0 * R1
            ((and (keywordp src) (keywordp dest))
                (let ((src-val (get-vm-registry vm src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (* dest-val src-val))))
            ;; 2) (MUL (:CONST 10) R0) => R0 = R0 * 10
            ((and (listp src) (keywordp dest))
                (let ((val (cadr src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (* dest-val val))))
            (t
             (error "Erreur d'argument pour l'instruction MUL: ~s" instruct)))))

(defun handle-div (vm instruct)
    "Gestion de l'instruction DIV <src> <dest>.
     - instruct doit être de la forme (DIV SRC DEST).
     - SRC est une valeur ou un registre.
     - DEST est TOUJOURS un registre."
        (let ((src  (cadr instruct))    ; 2e élément de la forme (DIV SRC DEST)
                (dest (caddr instruct)))  ; 3e élément
            (cond
            ;; 1) (DIV R1 R0) => R0 = R0 / R1
            ((and (keywordp src) (keywordp dest))
                (let ((src-val (get-vm-registry vm src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (/ dest-val src-val))))
            ;; 2) (DIV (:CONST 10) R0) => R0 = R0 / 10
            ((and (listp src) (keywordp dest))
                (let ((val (cadr src))
                    (dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (/ dest-val val))))
            (t
             (error "Erreur d'argument pour l'instruction DIV: ~s" instruct)))))

(defun handle-incr (vm instruct)
    "Gestion de l'instruction INCR <dest>.
     - instruct doit être de la forme (INCR DEST).
     - DEST est TOUJOURS un registre."
        (let ((dest (cadr instruct)))  ; 2e élément de la forme (INCR DEST)
            (cond
            ;; 1) (INCR R0) => R0 = R0 + 1
            ((keywordp dest)
                (let ((dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (+ dest-val 1))))
                (t
                    (error "Erreur d'argument pour l'instruction INCR: ~s" instruct)))
            )
        )


(defun handle-decr (vm instruct)
    "Gestion de l'instruction DECR <dest>.
     - instruct doit être de la forme (DECR DEST).
     - DEST est TOUJOURS un registre."
        (let ((dest (cadr instruct)))  ; 2e élément de la forme (DECR DEST)
            (cond
            ;; 1) (DECR R0) => R0 = R0 - 1
            ((keywordp dest)
                (let ((dest-val (get-vm-registry vm dest)))
                    (set-vm-registry vm dest (- dest-val 1))))
                (t
                    (error "Erreur d'argument pour l'instruction DECR: ~s" instruct)))))


