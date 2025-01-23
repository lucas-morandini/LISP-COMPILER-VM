;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nom du fichier : vm-type-check.lisp
;; Auteur         : MORANDINI Lucas
;; Date           : 22/01/2025
;; Description    : Définition des fonctions de vérification de type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-const (src)
  "Vérifie si src est une constante de la forme (:CONST <val>)"
  (and (listp src) (= (length src) 2) (eq (car src) :CONST)))

(defun is-var (src)
  "Vérifie si src est une variable de la forme '(@ <reg>)"
  (and (listp src) (= (length src) 2) (eq (car src) '@) (atom (cadr src))))

(defun is-offset (src)
  "Vérifie si src est un offset de la forme '(+ <reg> <offset>)"
  (and (listp src) (= (length src) 3) (eq (car src) '+) (keywordp (cadr src)) (integerp (caddr src))))

(defun is-ref (src)
  "Vérifie si src est une référence de la forme '(:REF <reg>)"
  (and (listp src) (= (length src) 2) (eq (car src) :REF) (keywordp (cadr src))))

(defun is-apply (src)
  "Vérifie si src est une application de fonction de la forme '(<fonc> <args>)"
  ;; t si car src est une clef dans la table des labels 
  (and (listp src) (not (is-jump-instruct src)) (not (is-halt-instruct src)) (not (is-cmp-instruct src)) (not (is-push-instruct src)) (not (is-pop-instruct src)) (not (is-arith-instruct src)) (not (is-test-instruct src)) (not (is-nop-instruct src)) (not (is-move-instruct src)) (not (is-load-instruct src)) (not (is-store-instruct src)) (not (is-var src)) (not (is-const src)) (not (is-offset src)) (not (is-ref src)))
)

;; --- Instructions ---
(defun is-jump-instruct (src)
  "Vérifie si src est une instruction de saut"
  (and (listp src) (member (car src) '(JMP JEQ JNE JLT JGT JLE JGE JNIL JTRUE JSR ))))

(defun is-halt-instruct (src)
  "Vérifie si src est une instruction HALT"
  (and (listp src) (eq (car src) 'HALT)))

(defun is-cmp-instruct (src)
  "Vérifie si src est une instruction de comparaison"
  (and (listp src) (eq (car src) 'CMP)))

(defun is-push-instruct (src)
  "Vérifie si src est une instruction de push"
  (and (listp src) (eq (car src) 'PUSH)))

(defun is-pop-instruct (src)
  "Vérifie si src est une instruction de pop"
  (and (listp src) (eq (car src) 'POP)))

(defun is-arith-instruct (src)
  "Vérifie si src est une instruction arithmétique"
  (and (listp src) (member (car src) '(ADD SUB MUL DIV INCR DECR))))

(defun is-test-instruct (src)
  "Vérifie si src est une instruction de test"
  (and (listp src) (eq (car src) 'TEST)))

(defun is-nop-instruct (src)
  "Vérifie si src est une instruction NOP"
  (and (listp src) (eq (car src) 'NOP)))

(defun is-move-instruct (src)
  "Vérifie si src est une instruction de mouvement"
  (and (listp src) (eq (car src) 'MOVE)))

(defun is-load-instruct (src)
  "Vérifie si src est une instruction de chargement"
  (and (listp src) (eq (car src) 'LOAD)))

(defun is-store-instruct (src)
  "Vérifie si src est une instruction de stockage"
  (and (listp src) (eq (car src) 'STORE)))

(defun is-valid-instruct (src)
  "Vérifie si src est une instruction valide"
  (or (is-jump-instruct src) (is-halt-instruct src) (is-cmp-instruct src) (is-push-instruct src) (is-pop-instruct src) (is-arith-instruct src) (is-test-instruct src) (is-nop-instruct src) (is-move-instruct src) (is-load-instruct src) (is-store-instruct src) (is-var src) ))
