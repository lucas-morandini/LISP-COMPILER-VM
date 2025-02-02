;;; ============================================================================
;;; Fichier : compiler-comp.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 23/01/2025
;;; Description : Fichier pour la compilation des expressions de comparaison
;;; ============================================================================

(defun compile-gt (expression)
  "Compile la comparaison superieur"
  (let ((label (genlabel))
        (end (genlabel)))
        (format t "first expression: ~A~%" (car expression))
        (format t "second expression: ~A~%" (cadr expression))
        (format t "third expression: ~A~%" (caddr expression))
    (append (compile-generic (car expression))
            (compile-generic (cadr expression))
            `((POP :R1) 
              (POP :R0) 
              (CMP :R0 :R1) 
              (JGT ,label) 
              (MOVE (:CONST t) :R0) 
              (JMP ,end) 
              (LABEL ,label) 
              (MOVE (:CONST nil) :R0) 
              (LABEL ,end)))))

(defun compile-lt (expression)
    "Compile la comparaison inferieur"
    (let ((label (genlabel))
          (end (genlabel))
          )
    (append (compile-generic (cadr expression))
         (compile-generic (caddr expression))
            `((POP :R1) 
            (POP :R0) 
            (CMP :R0 :R1) 
            (JLT ,label) 
            (MOVE (:CONST t) :R0) 
            (JMP ,end) 
            (LABEL ,label) 
            (MOVE (:CONST nil) :R0) 
            (LABEL ,end)))))

(defun compile-eq (expression)
    "Compile la comparaison egal"
    (let ((label (genlabel))
          (end (genlabel))
                      )
    (append (compile-generic (cadr expression))
         (compile-generic (caddr expression))
            `((POP :R1) 
            (POP :R0) 
            (CMP :R0 :R1) 
            (JEQ ,label) 
            (MOVE (:CONST t) :R0) 
            (JMP ,end) 
            (LABEL ,label) 
            (MOVE (:CONST nil) :R0) 
            (LABEL ,end)))))

(defun compile-neq (expression)
    "Compile la comparaison different"
    (let ((label (genlabel))
          (end (genlabel))
                      )
    (append (compile-generic (cadr expression))
         (compile-generic (caddr expression))
            `((POP :R1) 
            (POP :R0) 
            (CMP :R0 :R1) 
            (JNE ,label) 
            (MOVE (:CONST t) :R0) 
            (JMP ,end) 
            (LABEL ,label) 
            (MOVE (:CONST nil) :R0) 
            (LABEL ,end)))))

(defun compile-geq (expression)
    "Compile la comparaison superieur ou egal"
    (let ((label (genlabel))
          (end (genlabel))
                      )
    (append (compile-generic (cadr expression))
         (compile-generic (caddr expression))
            `((POP :R1) 
            (POP :R0) 
            (CMP :R0 :R1) 
            (JGE ,label) 
            (MOVE (:CONST t) :R0) 
            (JMP ,end) 
            (LABEL ,label) 
            (MOVE (:CONST nil) :R0) 
            (LABEL ,end)))))

(defun compile-leq (expression)
    "Compile la comparaison inferieur ou egal"
    (let ((label (genlabel))
          (end (genlabel))
                      )
    (append (compile-generic (cadr expression))
         (compile-generic (caddr expression))
            `((POP :R1) 
            (POP :R0) 
            (CMP :R0 :R1) 
            (JLE ,label) 
            (MOVE (:CONST t) :R0) 
            (JMP ,end) 
            (LABEL ,label) 
            (MOVE (:CONST nil) :R0) 
            (LABEL ,end)))))



