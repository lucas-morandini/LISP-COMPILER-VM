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

; (:LIT l) empile le littéral l
; (:VAR n) empile la valeur de la n-ième variable du bloc de pile courant
; (:SET-VAR n) affecte la valeur dépilée à la n-ième variable du bloc de pile courant

(defun handle-lit (vm instruct)
  "Interprète l’instruction (:LIT l).
   Empile le littéral l sur la pile."
  (let ((value (cadr instruct)))
    (vm-push vm value)))

(defun handle-var (vm instruct)
  "Interprète l’instruction (:VAR n).
   Empile la valeur de la n-ième variable (adresse BP + n)."
  (let* ((index (cadr instruct))
         (bp    (get-vm-bp vm))
         (addr  (+ bp index))
         (val   (get-vm-memory-at vm addr)))
    (vm-push vm val)))

(defun handle-set-var (vm instruct)
  "Interprète l’instruction (:SET-VAR n).
   Dépile un élément et l’affecte à la n-ième variable (adresse BP + n)."
  (let* ((index (cadr instruct))
         (bp    (get-vm-bp vm))
         (addr  (+ bp index))
         (val   (vm-pop vm)))
    (set-vm-memory-at vm addr val)))
