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

;; --- BP ---
(defun get-vm-bp (vm)
    (vm-bp vm))
(defun set-vm-bp (vm value)
    (setf (vm-bp vm) value))

;; --- SP ---
(defun get-vm-sp (vm)
    (vm-sp vm))
(defun set-vm-sp (vm value)
    (setf (vm-sp vm) value))

;; --- PC ---
(defun get-vm-pc (vm)
    (vm-pc vm))
(defun set-vm-pc (vm value)
    (setf (vm-pc vm) value))

;; --- FP ---
(defun get-vm-fp (vm)
    (vm-fp vm))
(defun set-vm-fp (vm value)
    (setf (vm-fp vm) value))

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

;; --- MEMORY ---
(defun get-vm-memory (vm)
    (vm-memory vm))
(defun set-vm-memory (vm value)
    (setf (vm-memory vm) value))
(defun get-vm-memory-at (vm index)
    (aref (vm-memory vm) index))
(defun set-vm-memory-at (vm index value)
    (setf (aref (vm-memory vm) index) value))

;; --- REGISTRY ---
(defun get-vm-registry (vm reg)
  (gethash reg (vm-registry vm)))
(defun set-vm-registry (vm reg value)
  (setf (gethash reg (vm-registry vm)) value))

