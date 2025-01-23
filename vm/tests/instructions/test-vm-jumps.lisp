;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : test-vm-jumps.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Tests unitaires pour les opérations de saut de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-handle-jmp ()
  "Teste le comportement de handle-jmp sur toutes les possibilités."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; (JMP 10) => PC = 11 (car cible + 1)
    (handle-jmp vm '(JMP 10))
    (assert (equal (get-vm-pc vm) 11))
    (format t "Test JMP 10 OK, PC = ~A~%" (get-vm-pc vm))
    (format t "test-handle-jmp passed~%")))

(defun test-handle-jsr ()
  "Teste le comportement de handle-jsr sur toutes les possibilités."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; (JSR 4) => PC = 5 (cible + 1), MEM[SP] = PC (avant JSR)
    (let ((initial-pc (get-vm-pc vm)))
      (handle-jsr vm '(JSR 4))
      (assert (equal (get-vm-pc vm) 5))
      (assert (equal (get-vm-memory-at vm (get-vm-sp vm)) (+ initial-pc 1)))
      (format t "Test JSR 4 OK, PC = ~A, MEM[SP] = ~A~%" (get-vm-pc vm) (get-vm-memory-at vm (get-vm-sp vm))))
    (format t "test-handle-jsr passed~%")))

(defun test-handle-rtn ()
  "Teste le comportement de handle-rtn sur toutes les possibilités."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    ;; (JSR 6) => PC = 7, MEM[SP] = PC (avant JSR)
    ;; (RTN) => PC = MEM[SP], SP décrémenté
    (let ((initial-pc (get-vm-pc vm)))
      (handle-jsr vm '(JSR 6))
      (handle-rtn vm '(RTN))
      (format t "initial-pc + 1 = ~A~%" (1+ initial-pc))
        (format t "get-vm-pc vm = ~A~%" (get-vm-pc vm))
      (assert (equal (get-vm-pc vm) (+ initial-pc 1)))
      (assert (equal (get-vm-sp vm) 64)) ; SP revient à la position initiale
      (format t "Test RTN OK, PC = ~A~%" (get-vm-pc vm))
      (format t "test-handle-rtn passed~%"))))

(defun test-handle-cmp ()
  "Teste le comportement de handle-cmp sur toutes les possibilités."
  (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
    (set-vm-registry vm :R0 10)
    (set-vm-registry vm :R1 5)
    ;; CMP R0 R1 => FLT = 0, FEQ = 0, FGT = 1
    (handle-cmp vm '(CMP :R0 :R1))
    (assert (equal (get-vm-flt vm) 0))
    (assert (equal (get-vm-feq vm) 0))
    (assert (equal (get-vm-fgt vm) 1))
    (format t "Test CMP R0 R1 OK, FLT = ~A, FEQ = ~A, FGT = ~A~%" (get-vm-flt vm) (get-vm-feq vm) (get-vm-fgt vm))
    ;; CMP R0 R0 => FLT = 0, FEQ = 1, FGT = 0
    (handle-cmp vm '(CMP :R0 :R0))
    (assert (equal (get-vm-flt vm) 0))
    (assert (equal (get-vm-feq vm) 1))
    (assert (equal (get-vm-fgt vm) 0))
    (format t "Test CMP R0 R0 OK, FLT = ~A, FEQ = ~A, FGT = ~A~%" (get-vm-flt vm) (get-vm-feq vm) (get-vm-fgt vm))
    ;; CMP R1 R0 => FLT = 1, FEQ = 0, FGT = 0
    (handle-cmp vm '(CMP :R1 :R0))
    (assert (equal (get-vm-flt vm) 1))
    (assert (equal (get-vm-feq vm) 0))
    (assert (equal (get-vm-fgt vm) 0))
    (format t "Test CMP R1 R0 OK, FLT = ~A, FEQ = ~A, FGT = ~A~%" (get-vm-flt vm) (get-vm-feq vm) (get-vm-fgt vm))
    (format t "test-handle-cmp passed~%")))


(defun test-handle-jlt ()
    "Teste le comportement de handle-jlt."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-flt vm 1) ;; FLT = 1
        (handle-jlt vm '(JLT 10))
        (assert (equal (get-vm-pc vm) 11))
        (format t "Test JLT OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jlt passed~%"))
    )

(defun test-handle-jle ()
    "Teste le comportement de handle-jle."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-flt vm 1) ;; FLT = 1
        (set-vm-feq vm 0) ;; FEQ = 0
        (handle-jle vm '(JLE 10))
        (assert (equal (get-vm-pc vm) 11))
        (set-vm-flt vm 0) ;; FLT = 0
        (set-vm-feq vm 1) ;; FEQ = 1
        (handle-jle vm '(JLE 20))
        (assert (equal (get-vm-pc vm) 21))
        (format t "Test JLE OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jle passed~%"))
    )

(defun test-handle-jgt ()
    "Teste le comportement de handle-jgt."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-fgt vm 1) ;; FGT = 1
        (handle-jgt vm '(JGT 15))
        (assert (equal (get-vm-pc vm) 16))
        (format t "Test JGT OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jgt passed~%"))
    )

(defun test-handle-jge ()
    "Teste le comportement de handle-jge."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-fgt vm 1) ;; FGT = 1
        (handle-jge vm '(JGE 20))
        (assert (equal (get-vm-pc vm) 21))
        (set-vm-fgt vm 0) ;; FGT = 0
        (set-vm-feq vm 1) ;; FEQ = 1
        (handle-jge vm '(JGE 25))
        (assert (equal (get-vm-pc vm) 26))
        (format t "Test JGE OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jge passed~%"))
    )

(defun test-handle-jeq ()
    "Teste le comportement de handle-jeq."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-feq vm 1) ;; FEQ = 1
        (handle-jeq vm '(JEQ 30))
        (assert (equal (get-vm-pc vm) 31))
        (format t "Test JEQ OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jeq passed~%"))
    )

(defun test-handle-jne ()
    "Teste le comportement de handle-jne."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-feq vm 0) ;; FEQ = 0
        (handle-jne vm '(JNE 35))
        (assert (equal (get-vm-pc vm) 36))
        (format t "Test JNE OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jne passed~%"))
    )

(defun test-handle-test ()
    "Teste le comportement de handle-test."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-registry vm :R0 nil)
        (handle-test vm '(TEST :R0))
        (assert (= (get-vm-fnil vm) 1))
        (set-vm-registry vm :R0 42)
        (handle-test vm '(TEST :R0))
        (assert (= (get-vm-fnil vm) 0))
        (format t "Test TEST OK, FNIL = ~A~%" (get-vm-fnil vm))
        (format t "test-handle-test passed~%"))
    )

(defun test-handle-jnil ()
    "Teste le comportement de handle-jnil."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-fnil vm 1) ;; FNIL = 1
        (handle-jnil vm '(JNIL 40))
        (assert (equal (get-vm-pc vm) 41))
        (format t "Test JNIL OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jnil passed~%"))
    )

(defun test-handle-jtrue ()
    "Teste le comportement de handle-jtrue."
    (let ((vm (init-vm :name "test-vm" :memsize 1024 :max-stack-size 256)))
        (set-vm-fnil vm 0) ;; FNIL = 0
        (handle-jtrue vm '(JTRUE 45))
        (assert (equal (get-vm-pc vm) 46))
        (format t "Test JTRUE OK, PC = ~A~%" (get-vm-pc vm))
        (format t "test-handle-jtrue passed~%"))
    )

(defun run-tests-jumps ()
    "Lance tous les tests pour les nouvelles instructions de saut."
    (test-handle-jmp)
    (test-handle-jsr)
    (test-handle-rtn)
    (test-handle-cmp)
    (test-handle-jlt)
    (test-handle-jle)
    (test-handle-jgt)
    (test-handle-jge)
    (test-handle-jeq)
    (test-handle-jne)
    (test-handle-test)
    (test-handle-jnil)
    (test-handle-jtrue))

;;(run-tests-jumps)

