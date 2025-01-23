(defun compile-symbol (expression)
  "Compile une variable => (:@ key)." ;; TODO : handling
  (format t "expression : ~a~%" expression)
    `((PUSH (:@ ,expression))))

(defun compile-atom (expression)
  "Compile une constante => (:CONST value)."
  `((PUSH (:CONST ,expression))))
