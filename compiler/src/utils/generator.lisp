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

(defvar *variable-table* (make-hash-table :test 'eq)
  "Table de hachage : symbol -> index.")

(defvar *next-var-index* 0
  "Compteur global pour attribuer un nouvel index aux variables.")

(defun lookup-or-create-var-index (var-symbol)
  "Renvoie l'index de la variable `var-symbol` dans la table. 
   Si `var-symbol` n'est pas encore référencé, crée une nouvelle entrée."
  (or (gethash var-symbol *variable-table*)
      (setf (gethash var-symbol *variable-table*)
            (prog1
                *next-var-index*
              (incf *next-var-index*)))))