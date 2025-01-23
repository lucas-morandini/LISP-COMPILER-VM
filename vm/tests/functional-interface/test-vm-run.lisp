;;; ============================================================================
;;; Fichier : test-vm-run.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests divers pour l'exécution de la VM
;;; ============================================================================

(require "src/instructions/vm-imports.lisp")
(require "src/utils/vm-imports.lisp")
(require "src/functional-interface/vm.lisp")

(defun test-run-vm-1 ()
  "Teste la fonction run-vm."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    (let ((code '((LABEL START)
                  (PUSH (:CONST 42))
                  (JMP END)
                  (PUSH (:CONST 99))
                  (LABEL END)
                  (HALT))))
      ;; Charger le code
      (load-vm vm code)
        ;; Exécuter le code
        (run-vm vm)
        ;; Vérifications
        ;; 1. PC mis à jour correctement
        (assert (equal (get-vm-pc vm) 326)) ; PC doit pointer à la fin du code
        ;; 2. SP mis à jour correctement
        (assert (equal (get-vm-sp vm) 65)) ; SP doit pointer à la fin de la pile
        ;; 3. Pile correctement modifiée
        (assert (equal (get-vm-memory-at vm 65) 42)) ; 42 doit être au sommet de la pile
        (assert (not(equal (get-vm-memory-at vm 66) 99))) ; 99 doit être ne pas etre empilé
      
      (format t "test vm-run passed~%"))))

(defun test-run-vm-fibo ()
  "Teste la fonction run-vm avec un code de calcul de Fibonacci."
  (let ((vm (init-vm :name "test-vm-fibo" :memsize 1024 :max-stack-size 256)))
    (let ((code '((LABEL START)
              ;; Initialiser n = 10 dans R0
              (MOVE (:CONST 10) :R0)
              ;; Initialiser F0 = 0 dans R1
              (MOVE (:CONST 0) :R1)
              ;; Initialiser F1 = 1 dans R2
              (MOVE (:CONST 1) :R2)
              ;; Début de la boucle
              (LABEL LOOP_START)
              ;; Si n <= 2, fin de boucle
              (CMP (:CONST 2) :R0)
              (JGT END_LOOP)
              ;; Calculer F(n) = F(n-1) + F(n-2)
              (PUSH :R2)
              (ADD :R1 :R2)      ;; :R2 = :R1 + :R2
              (POP :R1)          ;; :R1 = :R2
              ;; Décrémenter n
              (DECR :R0)
              ;; Recommencer la boucle
              (JMP LOOP_START)
              ;; Fin de la boucle
              (LABEL END_LOOP)
              (MOVE :R2 :R0)
              ;; Le résultat (Fibonacci) est dans :R2
              (HALT))))
  (load-vm vm code)
      ;; Exécuter le code
      (let ((result (run-vm vm)))
      (format t "Resultat de Fibonacci(10) = ~A~%" result)
        ;; Vérifications
        (assert (equal result 55)) ; Le 10ème terme de Fibonacci est 55
        (assert (equal (get-vm-pc vm) 335)) ; Vérifie que le PC est à la fin du programme
        (format t "test-run-vm-fibo passed~%")))))


(defun run-tests-run ()
  "Lance tous les tests pour le chargement de la VM."
  (test-run-vm-1)
  (test-run-vm-fibo))

; (run-tests-run)
(test-run-vm-fibo)