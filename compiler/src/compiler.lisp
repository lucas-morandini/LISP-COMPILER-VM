;;; ============================================================================
;;; Fichier : compiler.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier contenant le code du compilateur general
;;; ============================================================================

(require "compiler/src/expressions/compiler-symb-atom.lisp")
(require "compiler/src/expressions/compiler-arithm.lisp")
(require "compiler/src/expressions/compiler-comp.lisp")
(require "compiler/src/expressions/compiler-logique.lisp")
(require "compiler/src/expressions/compiler-ctrl-flux.lisp")
(require "compiler/src/expressions/compiler-fonc.lisp")
(require "compiler/src/expressions/compiler-var.lisp")
(require "compiler/src/utils/generator.lisp")
(require "vm/src/functional-interface/vm.lisp")
(require "vm/src/instructions/vm-imports.lisp")
(require "vm/src/utils/vm-imports.lisp")

(defun compile-generic (expression)
    "Table de correspondance des expressions"
(cond 
    ((symbolp expression) (compile-symbol expression)) ;; Symbol = variable
    ((atom expression) (compile-atom expression)) ;; Atom = constante
    ((listp expression)
        (cond
            ;; 1) Operation arithmetique
            ((eq (car expression) '+) (compile-add expression))
            ((eq (car expression) '-) (compile-sub expression))
            ((eq (car expression) '*) (compile-mul expression))
            ((eq (car expression) '/) (compile-div expression))
            ;; 2) Operation de comparaison
            ((eq (car expression) '>) (compile-gt expression))
            ((eq (car expression) '>=) (compile-ge expression))
            ((eq (car expression) '<) (compile-lt expression))
            ((eq (car expression) '<=) (compile-le expression))
            ((eq (car expression) '=) (compile-eq expression))
            ((eq (car expression) '!=) (compile-ne expression))
            ((eq (car expression) 'null) (compile-null expression))
            ;; 3) Operation logique
            ((eq (car expression) 'and) (compile-and expression))
            ((eq (car expression) 'or) (compile-or expression))
            ((eq (car expression) 'not) (compile-not expression))
            ;; 4) Operation de controle de flux
            ((eq (car expression) 'if) (compile-if expression))
            ((eq (car expression) 'while) (compile-while expression))
            ((eq (car expression) 'for) (compile-for expression))
            ((eq (car expression) 'do) (compile-do expression))
            ;; 5) Operation de definition de fonction
            ((eq (car expression) 'defun) (compile-defun expression))
            ;; 6) Operation de definition de variable
            ((eq (car expression) 'setq) (compile-setq expression))
            (t (error "Expression inconnue : ~a" expression))
        

        )))
)
