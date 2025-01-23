;;; ============================================================================
;;; Fichier : test-vm-load.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests divers pour le chargement de la VM
;;; ============================================================================

(require "src/instructions/vm-imports.lisp")
(require "src/utils/vm-imports.lisp")
(require "src/functional-interface/vm.lisp")

(defun test-load-vm-1 ()
  "Teste la fonction load-vm."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    (let ((code '((LABEL START)
                  (PUSH (:CONST 42))
                  (JMP END)
                  (PUSH (:CONST 99))
                  (LABEL END)
                  (HALT))))
      ;; Charger le code
      (load-vm vm code)
      ;; Afficher le contenu de la mémoire de 320 a 330
      (dotimes (i 5)
        (format t "MEM[~A] = ~A~%" (+ 320 i) (get-vm-memory-at vm (+ 320 i))))

      ;; Vérifications
      ;; 1. Labels enregistrés correctement
      (assert (equal (get-vm-label vm 'START) 320))
      (assert (equal (get-vm-label vm 'END) 324))

      ;; 2. Instructions correctement chargées
      (assert (equal (get-vm-memory-at vm 320) '(LABEL START)))      ; Label START
      (assert (equal (get-vm-memory-at vm 321) '(PUSH (:CONST 42)))) ; Instruction à START
      (assert (equal (get-vm-memory-at vm 322) '(JMP 324)))          ; JMP à END + 1
      (assert (equal (get-vm-memory-at vm 323) '(PUSH (:CONST 99)))) ; Instruction intermédiaire
      (assert (equal (get-vm-memory-at vm 324) '(LABEL END)))        ; Label END
      (assert (equal (get-vm-memory-at vm 325) '(HALT)))             ; HALT après END

      ;; 3. CP mis à jour correctement
      (assert (equal (get-vm-cp vm) 326)) ; CP doit pointer à la fin du code
      (format t "test vm-load passed~%"))))

(defun run-tests-load ()
  "Lance tous les tests pour le chargement de la VM."
  (test-load-vm-1))

;;(run-tests-load)