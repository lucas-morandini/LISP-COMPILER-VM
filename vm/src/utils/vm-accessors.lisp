;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-accessors.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition des accesseurs de la structure de la machine virtuelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Name ---
(defun get-vm-name (vm)
    (vm-name vm))
(defun set-vm-name (vm value)
    (setf (vm-name vm) value))

;; --- MEMORY ---
(defun get-vm-memory (vm)
    (vm-memory vm))
(defun set-vm-memory (vm value)
    (setf (vm-memory vm) value))
(defun get-vm-memory-at (vm index)
    (aref (vm-memory vm) index))
(defun set-vm-memory-at (vm index value)
    (setf (aref (vm-memory vm) index) value))

;; --- REGISTRies ---
(defun get-vm-registry (vm reg)
  (gethash reg (vm-registry vm)))
(defun set-vm-registry (vm reg value)
  (setf (gethash reg (vm-registry vm)) value))

;; --- POINTERS ---
;; --- BP : R4 ---
(defun get-vm-bp (vm)
    (get-vm-registry vm :R4))
(defun set-vm-bp (vm value)
    (set-vm-registry vm :R4 value))

;; --- SP : R7 ---
(defun get-vm-sp (vm)
    (get-vm-registry vm :R7))
(defun set-vm-sp (vm value)
    (set-vm-registry vm :R7 value))

;; --- PC : R5 ---
(defun get-vm-pc (vm)
    (get-vm-registry vm :R5))
(defun set-vm-pc (vm value)
    (set-vm-registry vm :R5 value))
;; --- FP : R6 ---
(defun get-vm-fp (vm)
    (get-vm-registry vm :R6))
(defun set-vm-fp (vm value)
    (set-vm-registry vm :R6 value))
;; --- VP ---
(defun get-vm-vp (vm)
    (vm-vp vm))
(defun set-vm-vp (vm value)
    (setf (vm-vp vm) value))

;; --- MAX-MEMORY-SIZE ---
(defun get-vm-max-memory-size (vm)
    (vm-max-memory-size vm))
(defun set-vm-max-memory-size (vm value)
    (setf (vm-max-memory-size vm) value))

;; --- MAX-STACK-SIZE ---
(defun get-vm-max-stack-size (vm)
    (vm-max-stack-size vm))
(defun set-vm-max-stack-size (vm value)
    (setf (vm-max-stack-size vm) value))

;; --- CP ---
(defun get-vm-cp (vm)
    (vm-cp vm))
(defun set-vm-cp (vm value)
    (setf (vm-cp vm) value))

;; --- FLAGS ---
;; --- FLT ---
(defun get-vm-flt (vm)
    (vm-flt vm))
(defun set-vm-flt (vm value)
    (setf (vm-flt vm) value))

;; --- FGT ---
(defun get-vm-fgt (vm)
    (vm-fgt vm))
(defun set-vm-fgt (vm value)
    (setf (vm-fgt vm) value))

;; --- FEQ ---
(defun get-vm-feq (vm)
    (vm-feq vm))
(defun set-vm-feq (vm value)
    (setf (vm-feq vm) value))

;; --- FNIL ---
(defun get-vm-fnil (vm)
    (vm-fnil vm))
(defun set-vm-fnil (vm value)
    (setf (vm-fnil vm) value))

;; --- HALTED ---
(defun get-vm-halted (vm)
    (vm-halted vm))
(defun set-vm-halted (vm value)
    (setf (vm-halted vm) value))

;; --- LABELS ---
(defun get-vm-labels (vm)
    (vm-labels vm))
(defun set-vm-labels (vm value)
    (setf (vm-labels vm) value))

(defun get-vm-label (vm label)
    (gethash label (vm-labels vm)))
(defun set-vm-label (vm label value)
    (setf (gethash label (vm-labels vm)) value))

;; --- VARS ---
(defun get-vm-vars (vm)
    (vm-vars vm))
(defun set-vm-vars (vm value)
    (setf (vm-vars vm) value))

(defun get-vm-var (vm var)
    (gethash var (vm-vars vm)))
(defun set-vm-var (vm var value)
    (setf (gethash var (vm-vars vm)) value))

;; --- FUNCTIONS ---
(defun get-vm-functions (vm)
    (vm-functions vm))
(defun set-vm-functions (vm value)
    (setf (vm-functions vm) value))

(defun get-vm-function (vm function)
    (gethash function (vm-functions vm)))
(defun set-vm-function (vm function value)
(setf (gethash function (vm-functions vm)) value))


