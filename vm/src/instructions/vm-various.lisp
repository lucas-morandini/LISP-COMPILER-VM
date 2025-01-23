;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-various.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Definition des operations diverses de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-nop (vm instruct)
    "Gestion de l'instruction NOP."
    (format t "NOP~%")
    )

(defun handle-halt (vm instruct)
    "Gestion de l'instruction HALT."
    (set-vm-halted vm 1)
)


(defun handle-set-var (vm instruct)
    "Gestion de l'instruction @ <var>.
    -Ajoute var à la hashtable des variables
    "
    (let ((var (cadr instruct)))
        (set-vm-var vm var 0)
        (format t "@ : ~a~%" var)
    )
    )



