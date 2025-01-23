(defun compile-symbol (expression)
  "Compile une variable => (:VAR index)."
  (let ((index (lookup-or-create-var-index expression)))
    `((:VAR ,index))))

(defun compile-atom (expression)
  "Compile un littéral => (:LIT valeur)."
  `((:LIT ,expression)))
