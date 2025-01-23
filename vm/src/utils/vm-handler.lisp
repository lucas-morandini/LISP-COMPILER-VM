;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-handler.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Handler générique pour les instructions de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handler (vm instruct)
    "Handler générique pour les instructions de la machine virtuelle"
    (cond
        ((is-jump-instruct instruct) (handle-jump vm instruct))
        ((is-halt-instruct instruct) (handle-halt vm instruct))
        ((is-cmp-instruct instruct) (handle-cmp vm instruct))
        ((is-push-instruct instruct) (handle-push vm instruct))
        ((is-pop-instruct instruct) (handle-pop vm instruct))
        ((is-arith-instruct instruct) (handle-arith vm instruct))
        ((is-test-instruct instruct) (handle-test vm instruct))
        ((is-nop-instruct instruct) (handle-nop vm instruct))
        ((is-mov-instruct instruct) (handle-mov vm instruct))
        ((is-load-instruct instruct) (handle-load vm instruct))
        (t (error "Instruction inconnue"))))