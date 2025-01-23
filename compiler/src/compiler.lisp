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

(defun compile-generic (expression)
    "Table de correspondance des expressions"
(cond 
    ((symbolp expression) (compile-symbol expression)) ;; Symbol = variable
    ((atom expression) (compile-atom expression)) ;; Atom = constante
    ((listp expression)
        (cond
            ;; 1) Operation arithmetique
            ((eq (car expression) '+) (compile-add (cdr expression)))
            ((eq (car expression) '-) (compile-sub (cdr expression)))
            ((eq (car expression) '*) (compile-mul (cdr expression)))
            ((eq (car expression) '/) (compile-div (cdr expression)))
            ;; 2) Operation de comparaison
            ((eq (car expression) '>) (compile-gt (cdr expression)))
            ((eq (car expression) '>=) (compile-ge (cdr expression)))
            ((eq (car expression) '<) (compile-lt (cdr expression)))
            ((eq (car expression) '<=) (compile-le (cdr expression)))
            ((eq (car expression) '=) (compile-eq (cdr expression)))
            ((eq (car expression) '!=) (compile-ne (cdr expression)))
            ((eq (car expression) 'null) (compile-null (cdr expression)))
            ;; 3) Operation logique
            ((eq (car expression) 'and) (compile-and (cdr expression)))
            ((eq (car expression) 'or) (compile-or (cdr expression)))
            ((eq (car expression) 'not) (compile-not (cdr expression)))
            ;; 4) Operation de controle de flux
            ((eq (car expression) 'if) (compile-if (cdr expression)))
            ((eq (car expression) 'while) (compile-while (cdr expression)))
            ((eq (car expression) 'for) (compile-for (cdr expression)))
            ((eq (car expression) 'do) (compile-do (cdr expression)))
            ;; 5) Operation de definition de fonction
            ((eq (car expression) 'defun) (compile-defun (cdr expression)))
            ;; 6) Operation de definition de variable
            ((eq (car expression) 'setq) (compile-setq (cdr expression)))

        )))
)
