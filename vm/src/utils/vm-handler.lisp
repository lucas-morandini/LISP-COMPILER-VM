;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-handler.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Handler générique pour les instructions de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handler (vm instruct)
    "Handler générique pour les instructions de la machine virtuelle"
    (cond
        ((is-jump-instruct instruct) 
            (cond
                ((eq (car instruct) 'JMP) (handle-jmp vm instruct))
                ((eq (car instruct) 'JEQ) (handle-jeq vm instruct))
                ((eq (car instruct) 'JNE) (handle-jne vm instruct))
                ((eq (car instruct) 'JLT) (handle-jlt vm instruct))
                ((eq (car instruct) 'JGT) (handle-jgt vm instruct))
                ((eq (car instruct) 'JLE) (handle-jle vm instruct))
                ((eq (car instruct) 'JGE) (handle-jge vm instruct))
                ((eq (car instruct) 'JNIL) (handle-jnil vm instruct))
                ((eq (car instruct) 'JTRUE) (handle-jtrue vm instruct))
                ((eq (car instruct) 'JSR) (handle-jsr vm instruct))
                (t (error "Instruction de saut inconnue")))
            )
        ((is-halt-instruct instruct) (handle-halt vm instruct))
        ((is-cmp-instruct instruct) (handle-cmp vm instruct))
        ((is-push-instruct instruct) (handle-push vm instruct))
        ((is-pop-instruct instruct) (handle-pop vm instruct))
        ((is-arith-instruct instruct)
            (cond
                ((eq (car instruct) 'ADD) (handle-add vm instruct))
                ((eq (car instruct) 'SUB) (handle-sub vm instruct))
                ((eq (car instruct) 'MUL) (handle-mul vm instruct))
                ((eq (car instruct) 'DIV) (handle-div vm instruct))
                ((eq (car instruct) 'INCR) (handle-incr vm instruct))
                ((eq (car instruct) 'DECR) (handle-decr vm instruct))
                (t (error "Instruction arithmétique inconnue")))
        )
        ((is-test-instruct instruct) (handle-test vm instruct))
        ((is-nop-instruct instruct) (handle-nop vm instruct))
        ((is-move-instruct instruct) (handle-move vm instruct))
        ((is-load-instruct instruct) (handle-load vm instruct))
        (t (error "Instruction inconnue"))))