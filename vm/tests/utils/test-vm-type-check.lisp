;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : test-vm-type-check.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Tests pour les vérifications de types dans vm-type-check.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/instructions/vm-imports.lisp")
(require "../src/utils/vm-imports.lisp")
(require "../src/functional-interface/vm.lisp")

(defun test-is-const ()
    "Teste le comportement de is-const sur toutes les possibilités"
    (assert (is-const '(:CONST 10)))
    (assert (not (is-const '(:CONST 10 20))))
    (assert (not (is-const :CONST)))
    (assert (not (is-const '(:CONST))))
    (assert (not (is-const '(:CONST 10 20 30))))
    (format t "test-is-const passed~%"))

(defun test-is-offset ()
    "Teste le comportement de is-offset sur toutes les possibilités"
    (assert (is-offset '(+ :R0 10)))
    (assert (not (is-offset '(+ :R0))))
    (assert (not (is-offset '(+ 10))))
    (assert (not (is-offset '(+ :R0 10 20))))
    (assert (not (is-offset '(+ :R0 :R1))))
    (format t "test-is-offset passed~%"))

(defun test-is-ref ()
    "Teste le comportement de is-ref sur toutes les possibilités"
    (assert (is-ref '(:REF :R0)))
    (assert (not (is-ref '(:REF))))
    (assert (not (is-ref '(:REF :R0 :R1))))
    (assert (not (is-ref :REF)))
    (assert (not (is-ref '(+ :R0 10))))
    (format t "test-is-ref passed~%"))

(defun test-instructions ()
    "Teste les vérifications d'instructions"
    ;; Sauts
    (assert (is-jump-instruct '(JMP :R0)))
    (assert (is-jump-instruct '(JEQ :LABEL)))
    (assert (not (is-jump-instruct '(ADD :R0 :R1))))
    (assert (not (is-jump-instruct :JMP)))
    (format t "test-is-jump-instruct passed~%")

    ;; HALT
    (assert (is-halt-instruct '(HALT)))
    (assert (not (is-halt-instruct '(:HALT))))
    (assert (not (is-halt-instruct :HALT)))
    (format t "test-is-halt-instruct passed~%")

    ;; CMP
    (assert (is-cmp-instruct '(CMP :R0 :R1)))
    (assert (not (is-cmp-instruct '(JMP :R0))))
    (format t "test-is-cmp-instruct passed~%")

    ;; PUSH
    (assert (is-push-instruct '(PUSH (:CONST 10))))
    (format t "test-is-push-instruct passed~%")

    ;; POP
    (assert (is-pop-instruct '(POP :R0)))
    (format t "test-is-pop-instruct passed~%")

    ;; Instructions arithmétiques
    (assert (is-arith-instruct '(ADD :R0 :R1)))
    (assert (is-arith-instruct '(SUB :R0 :R1)))
    (assert (not (is-arith-instruct '(CMP :R0 :R1))))
    (format t "test-is-arith-instruct passed~%")

    ;; TEST
    (assert (is-test-instruct '(TEST :R0)))
    (assert (not (is-test-instruct '(ADD :R0 :R1))))
    (format t "test-is-test-instruct passed~%")

    ;; NOP
    (assert (is-nop-instruct '(NOP)))
    (format t "test-is-nop-instruct passed~%")

    ;; MOV
    (assert (is-mov-instruct '(MOV :R0 :R1)))
    (format t "test-is-mov-instruct passed~%")

    ;; LOAD
    (assert (is-load-instruct '(LOAD :R0 :R1)))
    (format t "test-is-load-instruct passed~%")

    ;; STORE
    (assert (is-store-instruct '(STORE :R0 :R1)))
    (format t "test-is-store-instruct passed~%"))

(defun run-tests-type-check ()
    "Lance tous les tests de vérification de type"
    (test-is-const)
    (test-is-offset)
    (test-is-ref)
    (test-instructions))

;;(run-tests-type-check)
