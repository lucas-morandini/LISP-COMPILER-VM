;;; ============================================================================
;;; Fichier : generator.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour la generation de variables
;;; ============================================================================

(defun genlabel ()
    "Genere un label"
    (let ((label (gensym)))
        (format nil "Label~A" label)))
    
(defun genvar ()
    "Genere une variable"
    (let ((var (gensym)))
        (format nil "Var~A" var)))