;;; ============================================================================
;;; Fichier : test-vm-arithm.lisp
;;; Auteur : MORANDINI Lucas
;;; Date : 22/01/2025
;;; Description : Tests pour handle-move
;;; ============================================================================

(require "../src/instructions/vm-imports.lisp")
(require "../src/utils/vm-imports.lisp")
(require "../src/functional-interface/vm.lisp")

(defun test-handle-add ()
    "Teste le comportement de handle-add sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (ADD :R1 :R0) => :R0 = :R0 + :R1 = 10 + 20 = 30
        (set-vm-registry vm :R1 10)
        (set-vm-registry vm :R0 20)
        (handle-add vm '(ADD :R1 :R0))
        (assert (equal (get-vm-registry vm :R0) 30))
        (format t "Test ADD :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        ;; 2) (ADD (:CONST 10) :R0) => :R0 = :R0 + 10 = 30 + 10 = 40
        (set-vm-registry vm :R0 30)
        (handle-add vm '(ADD (:CONST 10) :R0))
        (assert (equal (get-vm-registry vm :R0) 40))
        (format t "Test ADD (:CONST 10) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-add passed~%"))
)

(defun test-handle-sub ()
    "Teste le comportement de handle-sub sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (SUB :R1 :R0) => :R0 = :R0 - :R1 = 20 - 10 = 10
        (set-vm-registry vm :R1 10)
        (set-vm-registry vm :R0 20)
        (handle-sub vm '(SUB :R1 :R0))
        (assert (equal (get-vm-registry vm :R0) 10))
        (format t "Test SUB :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        ;; 2) (SUB (:CONST 10) :R0) => :R0 = :R0 - 10 = 20 - 10 = 10
        (set-vm-registry vm :R0 20)
        (handle-sub vm '(SUB (:CONST 10) :R0))
        (assert (equal (get-vm-registry vm :R0) 10))
        (format t "Test SUB (:CONST 10) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-sub passed~%"))
)

(defun test-handle-mul ()
    "Teste le comportement de handle-mul sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (MUL :R1 :R0) => :R0 = :R0 * :R1 = 10 * 20 = 200
        (set-vm-registry vm :R1 10)
        (set-vm-registry vm :R0 20)
        (handle-mul vm '(MUL :R1 :R0))
        (assert (equal (get-vm-registry vm :R0) 200))
        (format t "Test MUL :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        ;; 2) (MUL (:CONST 10) :R0) => :R0 = :R0 * 10 = 20 * 10 = 200
        (set-vm-registry vm :R0 20)
        (handle-mul vm '(MUL (:CONST 10) :R0))
        (assert (equal (get-vm-registry vm :R0) 200))
        (format t "Test MUL (:CONST 10) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-mul passed~%"))
)

(defun test-handle-div ()
    "Teste le comportement de handle-div sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (DIV :R1 :R0) => :R0 = :R0 / :R1 = 20 / 10 = 2
        (set-vm-registry vm :R1 10)
        (set-vm-registry vm :R0 20)
        (handle-div vm '(DIV :R1 :R0))
        (assert (equal (get-vm-registry vm :R0) 2))
        (format t "Test DIV :R1 :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        ;; 2) (DIV (:CONST 10) :R0) => :R0 = :R0 / 10 = 20 / 10 = 2
        (set-vm-registry vm :R0 20)
        (handle-div vm '(DIV (:CONST 10) :R0))
        (assert (equal (get-vm-registry vm :R0) 2))
        (format t "Test DIV (:CONST 10) :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-div passed~%"))
)

(defun test-handle-incr ()
    "Teste le comportement de handle-incr sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (INCR :R0) => :R0 = :R0 + 1 = 20 + 1 = 21
        (set-vm-registry vm :R0 20)
        (handle-incr vm '(INCR :R0))
        (assert (equal (get-vm-registry vm :R0) 21))
        (format t "Test INCR :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-incr passed~%"))
)

(defun test-handle-decr ()
    "Teste le comportement de handle-decr sur toutes les possibilités"
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        ;; 1) (DECR :R0) => :R0 = :R0 - 1 = 20 - 1 = 19
        (set-vm-registry vm :R0 20)
        (handle-decr vm '(DECR :R0))
        (assert (equal (get-vm-registry vm :R0) 19))
        (format t "Test DECR :R0 OK, :R0 = ~A~%" (get-vm-registry vm :R0))

        (format t "test-handle-decr passed~%"))
)

(defun run-tests-arithm ()
    "Lance tous les tests de test-vm-registry"
    (test-handle-add)
    (test-handle-sub)
    (test-handle-mul)
    (test-handle-div)
    (test-handle-incr)
    (test-handle-decr)
)

;;(run-tests-arithm)
