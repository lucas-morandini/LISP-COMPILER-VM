# Projet : Compilation LISP vers VM

## Description Générale
Ce projet vise à concevoir et implémenter un ensemble de modules permettant la compilation et l'exécution de programmes LISP à l'aide d'une machine virtuelle (VM) dédiée. Il s'articule autour de deux grands modules :

1. **Générateur de code pour la VM** : Générer du code exécutable sur la machine virtuelle.  
2. **Machine Virtuelle (VM)** : Permettre l’exécution du code généré (y compris la gestion des fonctions LISP primitives et le chargement de code).

## Tests et Exemples 

### Compilation + chargement d'une opération arithmétique et d'une fonction simple
```lisp
(+ 1 2)
```
```lisp
(defun f (x) (+ x 1))
```
Lancer la commande suivante pour compiler le code LISP ci-dessus :
```bash
clisp compiler/tests/run-all-tests.lisp
```
Output dans le terminal.

### Chargement et execution de fibonacci(10) deja compilé dans la VM
``` bash
clisp vm/tests/functional-interface/test-vm-run.lisp 
```

