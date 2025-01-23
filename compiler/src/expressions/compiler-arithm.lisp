;;; ============================================================================
;;; Fichier : compiler-arithm.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour la compilation des expressions arithmetiques
;;; ============================================================================

(defun compile-add (expression)
    "Compile l'addition"
    (let ((left (cadr expression))
          (right (caddr expression)))
      (append (compile-generic left)
             (compile-generic right)
             '((POP :R1) (POP :R0) (ADD :R1 :R0) (PUSH :R0)))
    )
)

(defun compile-sub (expression)
    "Compile la soustraction"
    (let ((left (cadr expression))
          (right (caddr expression)))
      (append (compile-generic left)
             (compile-generic right)
             '((POP :R1) (POP :R0) (SUB :R1 :R0) (PUSH :R0)))
    )
)

(defun compile-mul (expression)
    "Compile la multiplication"
    (let ((left (cadr expression))
          (right (caddr expression)))
      (append (compile-generic left)
             (compile-generic right)
             '((POP :R1) (POP :R0) (MUL :R1 :R0) (PUSH :R0)))
    )
)

(defun compile-div (expression)
    "Compile la division"
    (let ((left (cadr expression))
          (right (caddr expression)))
      (append (compile-generic left)
             (compile-generic right)
             '((POP :R1) (POP :R0) (DIV :R1 :R0) (PUSH :R0)))
    )
)


